#include <tree_sitter/parser.h>
#include <vector>
#include <cwctype>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <functional>
#include <algorithm>

using namespace std;
using std::placeholders::_1;

/**
 * The scanner is abstracted for compositionality as functions of the type:
 *
 * typedef function<Result(State&)> Parser;
 *
 * A simple parser can look like this:
 *
 * Result layout_start_brace(State & state) {
 *   if (next_char(state) == '{') return result::success(Sym::start);
 *   else return result::cont;
 * }
 *
 * With the provided combinators in `namespace `parser`, this can be rewritten as:
 *
 * Parser layout_start_brace = peek('{')(success(Sym::start));
 *
 * In the API function `scan`, this parser can be executed:
 *
 * parser::eval(layout_start_brace, state);
 *
 * This will set the `lexer-result_symbol` accordingly and return a bool indicating success.
 *
 * Multiple parsers can be executed in succession with the plus operator:
 *
 * peek('w')(handle_w) + peek('i')(handle_i)
 *
 * If `handle_w` terminates with `result::success` or `result::fail` instead of `result::cont`, `handle_i` is not
 * executed.
 */

// --------------------------------------------------------------------------------------------------------
// Utilities
// --------------------------------------------------------------------------------------------------------

/**
 * Print input and result information.
 */
bool debug = false;

/**
 * Print the upcoming token after parsing finished.
 * Note: May change parser behaviour.
 */
bool debug_next_token = false;

/**
 * Print to stderr if the `debug` flag is `true`.
 */
struct Log {
  template<class A> void operator()(const A & msg) {
    if (debug) cerr << msg << endl;
  }
} logger;

struct Endl {} nl;

template<class A> Log & operator<<(Log & l, const A & a) {
  if (debug) cerr << a;
  return l;
}

Log & operator<<(Log & l, Endl) {
  if (debug) cerr << endl;
  return l;
}

template<class A, class B> A fst(pair<A, B> p) { return p.first; }

template<class A, class B, class C> function<C(A)> operator*(function<C(B)> f, function<B(A)> g) {
  return [=](A a) { return f(g(a)); };
}

template<class A, class B, class C> function<C(A)> operator*(function<C(B)> f, B (&g)(A)) {
  return [=](A a) { return f(g(a)); };
}

template<class A, class B, class C> function<C(A)> operator*(C (&f)(B), function<B(A)> g) {
  return [=](A a) { return f(g(a)); };
}

// --------------------------------------------------------------------------------------------------------
// Symbols
// --------------------------------------------------------------------------------------------------------

namespace syms {

/**
 * This enum is mapped to the `externals` list in the grammar according to how they are ordered (the names are
 * abitrary).
 *
 * When the `scan` function is called, the parameter `syms` contains a bool for each enum attribute indicating whether
 * the parse tree at the current position can accept the corresponding symbol.
 *
 * The attribute `fail` is not part of the parse tree, it is used to indicate that no matching symbol was found.
 *
 * The meanings are:
 *   - semicolon: An implicit end of a decl or statement, a newline in place of a semicolon
 *   - start: Start an implicit new layout after `where`, `do`, `of` or `in`, in place of an opening brace
 *   - end: End an implicit layout, in place of a closing brace
 *   - dot: For qualified modules `Data.List.null`, which have to be disambiguated from the `(.)` operator based on
 *     surrounding whitespace.
 *   - where: Parse an inline `where` token. This is necessary because `where` tokens can end layouts and it's necesary
 *     to know whether it is valid at that position, which can mean that it belongs to the last statement of the layout
 *   - splice: A TH splice starting with a `$`, to disambiguate from the operator
 *   - varsym: A symbolic operator
 *   - consym: A symbolic constructor
 *   - tyconsym: A symbolic type operator
 *   - comment: A line or block comment, because they interfere with operators, especially in QQs
 *   - cpp: A preprocessor directive. Needs to push and pop indent stacks
 *   - comma: Needed to terminate inline layouts like `of`, `do`
 *   - qq_start: Disambiguate the opening oxford bracket from list comprehension
 *   - empty: The empty file
 *   - fail: special indicator of failure
 */
enum Sym: uint16_t {
  semicolon,
  start,
  end,
  dot,
  where,
  splice,
  varsym,
  consym,
  tyconsym,
  comment,
  cpp,
  comma,
  qq_start,
  indent,
  empty,
  fail,
};

vector<string> names = {
  "semicolon",
  "start",
  "end",
  "dot",
  "where",
  "splice",
  "varsym",
  "consym",
  "tyconsym",
  "comment",
  "cpp",
  "comma",
  "qq_start",
  "indent",
  "empty",
};

string name(Sym t) { return t < names.size() ? names[t] : "unknown"; }

/**
 * The parser appears to call `scan` with all symbols declared as valid directly after it encountered an error, so
 * this function is used to detect them.
 */
bool all(const bool *syms) { return std::all_of(syms, syms + empty, [](bool a) { return a; }); }

/**
 * Append a symbol's string representation to the string `s` if it is valid.
 */
void add(string & s, const bool *syms, Sym t) {
  if (syms[t]) {
    if (!s.empty()) s += ",";
    s += name(t);
  }
}

/**
 * Produce a comma-separated string of valid symbols.
 */
string valid(const bool *syms) {
  if (syms::all(syms)) return "all";
  string result = "";
  for (Sym i = semicolon; i <= semicolon + empty; i = Sym(i + 1)) add(result, syms, i);
  return '"' + result + '"';
}

}

using syms::Sym;

// --------------------------------------------------------------------------------------------------------
// State
// --------------------------------------------------------------------------------------------------------

/**
 * This structure contains the external and internal state.
 *
 * The parser provides the lexer interface and the list of valid symbols.
 *
 * The internal state consists of a stack of indentation widths that is manipulated whenever a layout is started or
 * terminated.
 */
struct State {
  TSLexer *lexer;
  const bool *symbols;
  vector<vector<uint16_t>> & indents;

  State(TSLexer *l, const bool *vs, vector<vector<uint16_t>> & is):
    lexer(l),
    symbols(vs),
    indents(is)
  {}
};

const string format_indents(const State & state) {
  if (state.indents.empty() || state.indents.back().empty()) return "empty";
  string s;
  for (auto i : state.indents.back()) {
    if (!s.empty()) s += "-";
    s += std::to_string(i);
  }
  return s;
}

ostream & operator<<(ostream & out, const State & state) {
  return out << "State { syms = " << syms::valid(state.symbols) <<
    ", indents = " << format_indents(state) <<
    " }";
}

/**
 * These functions provide the basic interface to the lexer.
 * They are not defined as members for easier composition.
 */
namespace state {

/**
 * The parser's position in the current line.
 */
uint32_t column(State & state) { return state.lexer->get_column(state.lexer); }

/**
 * The next character that would be parsed.
 * Does not advance the parser position (consume the character).
 */
char next_char(State & state) { return static_cast<char>(state.lexer->lookahead); }

/**
 * Move the parser position one character to the right, treating the consumed character as part of the parsed token.
 */
void advance(State & state) { state.lexer->advance(state.lexer, false); }

/**
 * Move the parser position one character to the right, treating the consumed character as whitespace.
 */
void skip(State & state) { state.lexer->advance(state.lexer, true); }

void cpp_push(State & state) {
  if (state.indents.empty()) state.indents.push_back(vector<uint16_t>());
  else (state.indents.push_back(state.indents.back()));
}

void cpp_pop(State & state) { if (!state.indents.empty()) state.indents.pop_back(); }

}

// --------------------------------------------------------------------------------------------------------
// Condition
// --------------------------------------------------------------------------------------------------------

/**
 * This type abstracts over a boolean predicate of the current state.
 * It is used whenever a boolean condition should guard a nested parser.
 *
 * With the provided operator overloads, conditions can be logically combined without having to write lambdas for
 * passing along the `State`.
 *
 * TODO make this ( const State & -> char -> bool) and move the consuming part to Parser
 */
typedef function<bool(State&)> Condition;
typedef function<Condition(const char)> CharCondition;

Condition operator&(const Condition & l, const Condition & r) {
  return [=](State & state) { return l(state) && r(state); };
}

Condition operator|(const Condition & l, const Condition & r) {
  return [=](State & state) { return l(state) || r(state); };
}

Condition operator!(const Condition & c) { return [=](State & state) { return !c(state); }; }

CharCondition operator&(const CharCondition & l, const CharCondition & r) {
  return [=](char c) {
    return [=](State & state) {
     return l(c)(state) && r(c)(state);
    };
  };
}

CharCondition operator|(const CharCondition & l, const CharCondition & r) {
  return [=](char c) {
    return [=](State & state) {
     return l(c)(state) || r(c)(state);
    };
  };
}

CharCondition operator!(CharCondition con) {
  return [=](char c) {
    return [=](State & state) {
     return !con(c)(state); };
  };
}

/**
 * The set of conditions used in the parser implementation.
 */
namespace cond {

/**
 * Require that the next character matches a predicate, without advancing the parser.
 * Returns the next char as well.
 */
function<std::pair<bool, char>(State &)> peeks(function<bool(const char)> pred) {
  return [=](State & state) {
    auto c = state::next_char(state);
    auto res = pred(c);
    return std::make_pair(res, c);
  };
}

function<Condition(char)> eq(char c) {
  return [=](auto c1) { return [=](auto _) { return c == c1; }; };
}

/**
 * Require that the next character equals a concrete `c`, without advancing the parser.
 */
Condition peek(const char c) { return fst<bool, char> * peeks(std::bind(equal_to<const char>(), c, _1)); }

/**
 * Require that the next character matches a predicate, advancing the parser on success.
 */
function<std::pair<bool, char>(State &)> consumes(function<bool(char)> pred) {
  return [=](auto state) {
    auto res = peeks(pred)(state);
    if (res.first) { state::advance(state); }
    return res;
  };
}

/**
 * Require that the next character equals a concrete `c`, advancing the parser on success.
 */
Condition consume(const char c) { return fst<bool, char> * consumes(std::bind(equal_to<char>(), c, _1)); }

function<string(State &)> consume_while(function<Condition(char)> pred) {
  return [=](auto state) {
    string s = "";
    while (true) {
      auto c = state::next_char(state);
      auto res = pred(c)(state);
      if (!res || c == 0) break;
      state::advance(state);
      s += c;
    }
    return s;
  };
}

/**
 * Require that the argument symbol is valid for the current parse tree state.
 */
Condition sym(Sym t) { return [=](auto state) { return state.symbols[t]; }; }

/**
 * Require that the next character is whitespace (space or newline) without advancing the parser.
 */
Condition peekws = [](State & state) { return iswspace(state::next_char(state)); };

/**
 * Require that the next character is end-of-file.
 */
Condition peekeof = peek(0);

/**
 * Require that the argument string follows the current position, consuming all characters.
 * Note: This leaves characters from a partial match consumed, there is no way to backtrack the parser.
 */
Condition seq(const string & s) {
  return [=](auto state) { return all_of(s.begin(), s.end(), [&](auto a) { return consume(a)(state); }); };
}

/**
 * A token like a varsym can be terminated by whitespace of brackets.
 */
Condition token_end =
  peekeof | peekws | peek(')') | peek(']') | peek('[') | peek('(');

/**
 * Require that the argument string follows the current position and is followed by whitespace.
 * See `seq`
 */
Condition token(const string & s) { return seq(s) & token_end; }

/**
 * Require that the stack of layout indentations is not empty.
 * This is mostly used for safety.
 */
const bool indent_exists(const State & state) {
  if (state.indents.empty()) state.indents.push_back(vector<uint16_t>());
  return !state.indents.back().empty();
};

/**
 * Helper function for executing a condition callback with the current indentation.
 */
Condition check_indent(function<bool(uint16_t)> f) {
  return [=](auto state) { return indent_exists(state) && f(state.indents.back().back()); };
}

/**
 * Require that the current line's indent is greater or equal than the containing layout's, so the current layout is
 * continued.
 */
Condition keep_layout(uint16_t indent) { return check_indent([=](auto i) { return indent >= i; }); }

/**
 * Require that the current line's indent is equal to the containing layout's, so the line may start a new `decl`.
 */
Condition same_indent(uint32_t indent) { return check_indent([=](auto i) { return indent == i; }); }

/**
 * Require that the current line's indent is smaller than the containing layout's, so the layout may be ended.
 */
Condition smaller_indent(uint32_t indent) { return check_indent([=](auto i) { return indent < i; }); }

/**
 * Composite condition examining whether the current layout can be terminated if the line after the position where the
 * scan started begins with a `where`.
 *
 * This is needed because `where` can appear on the same indent as, for example, a `do` statement in a `decl`, while
 * being part of the latter and therefore having to end the `do`'s layout before parsing the `where`.
 *
 * This does only check whether the line begins with a `w`, the entire `where` is consumed by the calling parser below.
 */
Condition is_newline_where(uint32_t indent) {
  return keep_layout(indent) & (sym(Sym::semicolon) | sym(Sym::end)) & (!sym(Sym::where)) & peek('w');
}

CharCondition is_char(const char target) { return [=](const char c) { return [=](auto _){ return c == target; }; }; }

CharCondition newline = is_char('\n');

/**
 * Require that the state has not been initialized after parsing has started.
 *
 * This is necessary to handle a nonexistent `module` declaration.
 */
bool uninitialized(const State & state) { return !indent_exists(state); }

Condition column(uint32_t col) {
  return [=](auto state) { return state::column(state) == col; };
}

/**
 * Require that the parser determined an error in the previous step (see `syms::all`).
 */
bool after_error(const State & state) { return syms::all(state.symbols); }

bool symbolic(const char c) {
  switch (c) {
    case ':':
    case '!':
    case '#':
    case '$':
    case '%':
    case '&':
    case '*':
    case '+':
    case '.':
    case '/':
    case '<':
    case '=':
    case '>':
    case '?':
    case '@':
    case '\\':
    case '^':
    case '|':
    case '-':
    case '~':
      return true;
    default:
      return false;
  }
}

bool valid_varsym_one_char(const char c) {
  switch (c) {
    case '!':
    case '#':
    case '%':
    case '&':
    case '*':
    case '+':
    case '/':
    case '<':
    case '>':
    case '?':
    case '^':
    case '-':
    case '.':
    case '$':
      return true;
    default:
      return false;
  }
}

// TODO remove `!` again after fixing strictness
bool valid_tyconsym_one_char(const char c) {
  switch (c) {
    case '!':
    case '*':
      return false;
    case '~':
    case ':':
      return true;
    default:
      return valid_varsym_one_char(c);
  }
}

bool valid_symop_two_chars(const Sym s, const char first_char, const char second_char) {
  switch (first_char) {
    case '-':
      return second_char != '-' && second_char != '>';
    case '=':
      return second_char != '>';
    case '<':
      return second_char != '-';
    case '.':
      return second_char != '.';
    case ':':
      return second_char != ':';
    default:
      return true;
  }
}

bool symop_needs_ws(const char c) {
  switch (c) {
    case '$':
    case '?':
      return true;
    default:
      return false;
  }
}

Condition varid_start_char(const char c) { return [=](auto _) { return c == '_' || islower(c); }; }

Condition varid_char(const char c) { return [=](auto _) { return c == '_' || c == '\'' || isalnum(c); }; };

}

// --------------------------------------------------------------------------------------------------------
// Result
// --------------------------------------------------------------------------------------------------------

/**
 * Returned by a parser, indicating whether to continue with the next parser (`finished`) which symbol to select when
 * successful (`sym`).
 *
 * Whether parsing was successful is indicated by which symbol is selected – `Sym::fail` signals failure.
 */
struct Result {
  Sym sym;
  bool finished;
  Result(Sym s, bool f): sym(s), finished(f) {}
};

template<class A> ostream & operator<<(ostream & out, const Result & res) {
  out << "Result { finished = " << res.finished;
  if (res.finished) out << ", " << "result = " << syms::name(res.sym);
  return out << " }";
}

/**
 * Constructors for the continue, failure and success results.
 */
namespace result {

Result cont = Result(Sym::fail, false);
// TODO rename to `finish`
Result success(Sym t) { return Result(t, true); }
Result fail = success(Sym::fail);

}

// --------------------------------------------------------------------------------------------------------
// Parser
// --------------------------------------------------------------------------------------------------------

namespace parser {

/**
 * The main function shape for all parser combinators.
 */
typedef function<Result(State&)> Parser;

/**
 * Convenience alias for a function that attaches conditions to a parser.
 */
typedef function<Parser(Parser)> Modifier;

/**
 * Combinators that manipulate the state without producing a value or parse result.
 */
typedef function<void(State&)> Effect;

/**
 * Monadic bind for `Parser`. (>>=)
 */
template<class A> function<Parser(function<Parser(A)>)> with(A (&fa)(State &)) {
  return [&](function<Parser(A)> f) {
    return [=](auto state) {
      auto a = fa(state);
      return f(a)(state);
    };
  };
}

/**
 * Variant of `with` that discards the left operand's result. (>>)
 *
 * Semantics are "execute the right parser if the left parser doesn't fail".
 */
Parser operator+(Parser fa, Parser fb) {
  return [=](auto state) {
    auto res = fa(state);
    return res.finished ? res : fb(state);
  };
}

/**
 * Depending on the result of a condition, execute one of the supplied parsers.
 */
Parser either(Condition c, Parser match, Parser nomatch) {
  return [=](auto state) { return c(state) ? match(state) : nomatch(state); };
}

/**
 * Helper for lifting `Result` into a parser.
 */
Parser const_(Result r) { return [=](auto state) { return r; }; }

/**
 * Lazy evaluation for recursion.
 */
Parser lazy(function<Parser()> p) {
  return [=](auto state) { return p()(state); };
}

/**
 * Execute an `Effect`, then continue.
 */
Parser effect(Effect eff) {
  return [=](auto state) {
    eff(state);
    return result::cont;
  };
}

/**
 * Parser that terminates the execution with the successful detection of the given symbol.
 */
Parser success(const Sym s, string desc) {
  return [=](auto _) {
    logger << "success: " << desc << nl;
    return result::success(s);
  };
}

/**
 * Parser that terminates the execution unsuccessfully;
 */
Parser fail = const_(result::fail);

/**
 * Require a condition to be true for the next parser to be executed.
 *
 * If the condition is false, parsing continues after the skipped parser.
 *
 * This function returns a function, so it is applied with two parameter lists:
 *
 * iff(cond::after_error)(fail)
 */
Modifier iff(const Condition & c) {
  return [=](const Parser & chk) {
    return [=](auto state) {
      if (c(state)) return chk(state);
      else return result::cont;
    };
  };
}

/**
 * Require a plain `bool` to be true for the next parser to be executed.
 */
Modifier when(const bool c) { return iff([=] (auto _) { return c; }); }

/**
 * Require the given symbol to be valid for the next parser to be executed.
 */
Modifier sym(const Sym s) { return iff(cond::sym(s)); }

/**
 * Parser that terminates the execution with the successful detection of the given symbol, but only if it is expected.
 */
Parser success_sym(const Sym s, string desc) { return sym(s)(success(s, desc)); }

Modifier peek(const char c) { return iff(cond::peek(c)); }

/**
 * :: (bool -> Parser) -> (char -> bool) -> Parser
 *
 * If the predicate for the next character is true, pass the consumed character to the next parser.
 */
function<Parser(function<Parser(char)>)> peeks(function<bool(char)> pred) {
  return [=](auto next) {
    return [=](auto state) {
      auto res = cond::peeks(pred)(state);
      if (res.first) return next(res.second)(state);
      else return result::cont;
    };
  };
}

/**
 * Require the next character to be `c` for the next parser to be executed, advancing the lexer in the success case.
 */
Modifier consume(const char c) { return iff(cond::consume(c)); }

/**
 * :: (bool -> Parser) -> (char -> bool) -> Parser
 *
 * If the predicate for the next character is true, advance the lexer and pass the consumed character to the next
 * parser.
 */
function<Parser(function<Parser(char)>)> consumes(function<bool(char)> pred) {
  return [=](auto next) {
    return [=](auto state) {
      auto res = cond::consumes(pred)(state);
      if (res.first) return next(res.second)(state);
      else return result::cont;
    };
  };
}

Parser consume_while(function<Condition(char)> pred) { return effect(cond::consume_while(pred)); }

/**
 * Advance the lexer.
 */
Parser advance = effect(state::advance);

/**
 * Skip whitespace.
 */
Parser skip_ws = effect([](auto state) { while (cond::peekws(state)) state::skip(state); });

Modifier seq(string s) { return iff(cond::seq(s)); }

/**
 * Require the next characters to be equal to `s` for the next parser to be executed, advancing the lexer as far as the
 * characters match, even if not all of them match.
 */
Modifier token(string s) { return iff(cond::token(s)); }

/**
 * Instruct the lexer that the current position is the end of the potentially detected symbol, causing the next run to
 * be started after this character in the success case.
 *
 * This is useful if the validity of the detected symbol depends on what follows, e.g. in the case of a layout end
 * before a `where` token.
 */
Parser mark = effect([](auto state) { state.lexer->mark_end(state.lexer); });

/**
 * If the parser returns `cont`, fail.
 */
Parser or_fail(Parser chk) { return chk + fail; }

/**
 * Require the next character to be whitespace for the next parser to be executed, not advancing the lexer.
 */
Modifier peekws = iff(cond::peekws);

/**
 * Add one level of indentation to the stack, caused by starting a layout.
 */
Parser push(uint16_t ind) { return effect([=](auto state) {
  logger << "push: " << ind << nl;
  if (!state.indents.empty()) state.indents.back().push_back(ind);
}); }

/**
 * Remove one level of indentation from the stack, caused by the end of a layout.
 */
Parser pop =
  iff(cond::indent_exists)(effect([](auto state) {
    logger("pop");
    if(cond::indent_exists(state)) state.indents.back().pop_back();
  }));

/**
 * Advance the lexer until the following character is neither space nor tab.
 */
Parser skipspace =
  effect([](auto state) { while (cond::peek(' ')(state) || cond::peek('\t')(state)) state::skip(state); });

/**
 * If a layout end is valid at this position, remove one indentation layer and succeed with layout end.
 */
Parser layout_end(string desc) { return sym(Sym::end)(effect(pop) + success(Sym::end, desc)); }

/**
 * Convenience parser, since those two are often used together.
 */
Parser end_or_semicolon(string desc) { return layout_end(desc) + success_sym(Sym::semicolon, desc); }

}

// --------------------------------------------------------------------------------------------------------
// Logic
// --------------------------------------------------------------------------------------------------------

/**
 * These parsers constitute the higher-level logic, loosely.
 */
namespace logic {

using namespace parser;

/**
 * Advance the parser until a non-whitespace character is encountered, while counting whitespace according to the rules
 * in the syntax reference, resetting the counter on each newline.
 *
 * This advances to the first non-white character in the next nonempty line and determines its indentation.
 */
uint32_t count_indent(State & state) {
  uint32_t indent = 0;
  for (;;) {
    if (cond::consume('\n')(state)) {
      indent = 0;
    } else if (cond::consume(' ')(state)) {
      indent++;
    } else if (cond::consume('\t')(state)) {
      indent += 8;
    } else break;
  }
  return indent;
}

/**
 * End-of-file check.
 *
 * If EOF has been reched, two scenarios are valid:
 *  - The file is empty, in which case the parser is still at the root rule, where `Sym::empty` is valid.
 *  - The current layout can be ended. This may happen multiple times, since the parser will restart until the last
 *    layout end rule has been parsed.
 *
 * If those cases do not apply, parsing fails.
 */
Parser eof = peek(0)(sym(Sym::empty)(success(Sym::empty, "eof")) + end_or_semicolon("eof") + fail);

/**
 * Push the initial indentation at the beginning of the file or module decl to the column of first nonwhite character,
 * then succeed with the dummy symbol `Sym::indent`.
 */
Parser initialize(uint32_t column) {
  return when(column == 0)(skip_ws + mark + push(column) + success(Sym::indent, "init"));
}

/**
 * Ensure that the first token of the file isn't `module`, then initialize the indentation stack.
 *
 * If there is a `module`, this will succeed when the parser is on the next line.
 */
Parser initialize_without_module =
  iff(cond::uninitialized)(token("module")(fail) + with(state::column)(initialize));

/**
 * If a dot is neither preceded nor succeded by whitespace, it may be parsed as a qualified module dot.
 *
 * The preceding space is ensured by sequencing this parser before `skipspace` in `init`.
 * Since this parser cannot look back to see whether the preceding name is a conid, this has to be ensured by the
 * grammar, represented here by the requirement of a valid symbol `Sym::dot`.
 *
 * Since the dot is consumed here, the alternative interpretation, a `Sym::varsym`, has to be emitted here.
 * A `Sym::tyconsym` is invalid here, because the dot is only expected in expressions.
 */
Parser dot = sym(Sym::dot)(consume('.')(peekws(success_sym(Sym::varsym, "dot")) + mark + success(Sym::dot, "dot")));

Parser cpp_consume =
  [](auto state) {
    auto p =
      consume_while(!cond::newline & !cond::is_char('\\')) +
      consume('\\')(peek('\n')(parser::advance) + cpp_consume);
    return p(state);
  };

Parser cpp =
  consume('#')(
    seq("if")(effect(state::cpp_push)) +
    consume('e')(
      seq("ndif")(effect(state::cpp_pop)) +
      seq("l")(effect(state::cpp_pop) + effect(state::cpp_push))
    ) +
    cpp_consume +
    mark +
    success(Sym::cpp, "cpp")
  );

Parser cpp_init =
  iff(cond::column(0))(cpp);

/**
 * End a layout by removing an indentation from the stack, but only if the current column (which should be in the next
 * line after skipping whitespace) is smaller than the layout indent.
 */
Parser dedent(uint32_t indent) { return iff(cond::smaller_indent(indent))(mark + layout_end("dedent")); }

/**
 * Succeed if a `where` on a newline can end a statement or layout (see `is_newline_where`).
 *
 * This is the case after `do` or `of`, where the `where` can be on the same indent.
 */
Parser newline_where(uint32_t indent) {
  return iff(cond::is_newline_where(indent))(mark + token("where")(end_or_semicolon("newline_where")) + fail);
}

/**
 * Succeed for `Sym::semicolon` if the indent of the next line is equal to the current layout's.
 */
Parser newline_semicolon(uint32_t indent) {
  return sym(Sym::semicolon)(iff(cond::same_indent(indent))(mark + success(Sym::semicolon, "newline_semicolon")));
}

/**
 * Parse an inline `where` token.
 *
 * Necessary because `is_newline_where` needs to know that no `where` may follow.
 */
Parser where = token("where")(sym(Sym::where)(mark + success(Sym::where, "where")) + layout_end("where"));

/**
 * An `in` token ends the layout openend by a `let`.
 */
Parser in = token("in")(end_or_semicolon("in"));

/**
 * Detect the start of a quasiquote: An opening bracket followed by an optional varid and a vertical bar, all without
 * whitespace in between.
 */
Parser qq_start =
  parser::advance +
  mark +
  consume_while(cond::varid_start_char) +
  consume_while(cond::varid_char) +
  peek('|')(success(Sym::qq_start, "qq_start"))
  ;

/**
 * When an opening bracket was encountered, this parser tests for a quasiquote or fails, for the grammar to parser the
 * bracket literally.
 */
Parser bracket_open =
  sym(Sym::qq_start)(qq_start) + fail;

/**
 * When a dollar is not followed by whitespace, parse a splice.
 *
 * TODO non-parens splices can only be varids, apparently
 */
Parser splice =
  iff(!cond::token_end | cond::peek('('))(mark + success_sym(Sym::splice, "splice") + fail);

Parser inline_comment =
  consume_while(!cond::newline) + mark + success(Sym::comment, "inline_comment");

Parser symbolic_multi1(const Sym s, bool all_dashes);

/**
 * Recurse while the next character is symbolic, then successd with `Sym::varsym`.
 * TODO use a `skipwhile` combinator
 */
function<Parser(char)> symbolic_multi(const Sym s, bool all_dashes) {
  return [=](const char next) {
    auto still_dashes = all_dashes && next == '-';
    return symbolic_multi1(s, still_dashes);
  };
};

Parser symbolic_multi1(const Sym s, bool all_dashes) {
  return
    consumes(cond::symbolic)(symbolic_multi(s, all_dashes)) +
    when(all_dashes)(inline_comment) +
    mark +
    success(s, "symbolic_multi1")
    ;
}

/**
 * Succeed if the two symbolic characters don't match a reserved operator, otherwise fail.
 */
Parser two_symbols(const Sym s, const char first_char, const char second_char, bool both_dashes) {
  return
    when(both_dashes)(inline_comment) +
    when(cond::valid_symop_two_chars(s, first_char, second_char))(mark + success(s, "two_symbols")) +
    fail;
}

/**
 * After having consumed two symbolic characters, check whether the next character is symbolic as well.
 * If it is, the result is at least three characters long and therefore cannot match a reserved operator, so any
 * following symbolic characters will be consumed.
 * If it is not, check whether the two characters match a reserved operator.
 */
function<Parser(const char)> symbolic_c2(const Sym s, const char first_char) {
  return [=](const char second_char) {
    auto both_dashes = first_char == '-' && second_char == '-';
    return
      parser::advance +
      either(
        fst<bool, char> * cond::peeks(cond::symbolic),
        symbolic_multi1(s, both_dashes),
        two_symbols(s, first_char, second_char, both_dashes)
      );
  };
}

/**
 * Succeed if the symbolic character doesn't match a reserved operator, otherwise fail.
 */
Parser single_varsym(const char c) {
  return when(cond::valid_varsym_one_char(c))(
    mark +
    when(cond::symop_needs_ws(c))(
      when(c == '$')(splice) +
      iff(cond::token_end)(success(Sym::varsym, "single_varsym")) +
      fail
    ) + success(Sym::varsym, "single_varsym")
  ) + fail;
}

/**
 * Check whether the first char is symbolic and not a `:` (constructor marker).
 * If so, check whether the next character is symbolic as well.
 * If it is, continue with `varsym_c2`.
 * If it is not, check whether the character matches a reserved operator.
 *
 * TODO use consumes(valid_first_varsym) to call this, then again to call the next (`eithers`)
 * TODO generalize all parsers to take one character as arg
 */
Parser varsym(const char first_char) {
  return when(first_char != ':' && cond::symbolic(first_char))(
    parser::advance +
    either(
      fst<bool, char> * cond::peeks(cond::symbolic),
      with(state::next_char)(symbolic_c2(Sym::varsym, first_char)),
      single_varsym(first_char)
    )
  );
}

Parser consym(const char first_char) {
  return when(first_char == ':')(
    parser::advance +
    either(
      fst<bool, char> * cond::peeks(cond::symbolic),
      with(state::next_char)(symbolic_c2(Sym::consym, first_char)),
      mark + success(Sym::consym, "consym")
    )
  );
}

/**
 * Succeed if the symbolic character doesn't match a reserved operator for tycons, otherwise fail.
 */
Parser single_tyconsym(const char c) {
  return when(cond::valid_tyconsym_one_char(c))(
    mark +
    when(cond::symop_needs_ws(c))(iff(cond::token_end)(success(Sym::tyconsym, "single_tyconsym")) + fail) +
    success(Sym::tyconsym, "single_tyconsym")
  ) + fail;
}

Parser tyconsym(const char first_char) {
  return when(cond::symbolic(first_char))(
    parser::advance +
    either(
      fst<bool, char> * cond::peeks(cond::symbolic),
      with(state::next_char)(symbolic_c2(Sym::tyconsym, first_char)),
      single_tyconsym(first_char)
    )
  );
}

/**
 * To be called when it is certain that two minuses cannot succeed as a symbolic operator.
 * Those cases are:
 *   - `Sym::start` is valid
 *   - Operator matching was done already
 */
Parser minus =
  seq("--")(symbolic_multi1(Sym::fail, true) + fail);

Parser multiline_comment(uint16_t);

Parser multiline_comment_check(uint16_t level) {
  return [=](auto state) {
    auto c =
      iff(cond::peekeof)(fail) +
      seq("{-")(multiline_comment(level + 1) + fail) +
      seq("-}")(when(level <= 1)(mark + success(Sym::comment, "multiline_comment_check")) + multiline_comment(level - 1) + fail) +
      parser::advance + multiline_comment(level)
      ;
    return c(state);
  };
}

Parser multiline_comment(uint16_t level) {
  return consume_while(!cond::eq('{') & !cond::eq('-') & !cond::eq(0)) + multiline_comment_check(level) + fail;
}

Parser brace =
  iff(cond::seq("{-"))(
    iff(!cond::consume('#'))(multiline_comment(1))
  ) + fail;

Parser comment(const char next) {
  return when(next == '-')(minus + fail) + when(next == '{')(brace);
}

Parser close_layout_in_list(const char next) {
  return
    when(next == ']')(layout_end("bracket")) +
    when(next == ',')(
      sym(Sym::comma)(parser::advance + mark + success(Sym::comma, "commma")) +
      layout_end("comma")
    );
}

/**
 * Parse special tokens before the first newline that can't be reliably detected by tree-sitter:
 *
 *   - `where` here is just for the actual valid token
 *   - `in` closes a layout when inline
 *   - `)` can end the layout of an `of`
 *   - symbolic operators are complicated to implement with regex
 *   - `$` can be a splice if not followed by whitespace
 *   - '[' can be a list or a quasiquote
 */
Parser inline_tokens(const char next) {
  return
    when(next == 'w')(where + fail) +
    when(next == 'i')(in + fail) +
    when(next == ')')(end_or_semicolon(")")) +
    when(next == '[')(bracket_open) +
    sym(Sym::consym)(consym(next)) +
    sym(Sym::tyconsym)(tyconsym(next)) +
    sym(Sym::varsym)(varsym(next)) +
    comment(next) +
    sym(Sym::splice)(consume('$')(splice)) +
    close_layout_in_list(next)
    ;
}

/**
 * If the symbol `Sym::start` is valid, starting a new layout is almost always indicated.
 *
 * If the next character is a left brace, it is either a comment, pragma or an explicit layout. In the comment case, the
 * it must be parsed here.
 * If the next character is a minus, it might be a comment.
 *
 * In all of those cases, the layout can't be started now. In the comment and pragma case, it will be started in the
 * next run.
 *
 * This pushes the indentation of the first non-whitespace character onto the stack.
 */
Parser layout_start(uint32_t column) {
  return sym(Sym::start)(
    peek('{')(brace) +
    peek('-')(minus) +
    push(column) +
    success(Sym::start, "layout_start")
  );
}

/**
 * After a layout has ended, the originator might need to be terminated by semicolon as well, but since the layout end
 * advances until the next line, it cannot be done in the newline checks.
 *
 * This can happen, for example, with nested `do` layouts:
 *
 * f = do
 *   a <- b
 *   do c <- d
 *      e
 *   f
 *
 * Here, when the inner `do`'s  layout is ended, the next step is started at `f`, but the outer `do`'s layout expects a
 * semicolon. Since `f` is on the same indent as the outer `do`'s layout, this parser matches.
 */
Parser post_end_semicolon(uint32_t column) {
  return sym(Sym::semicolon)(iff(cond::same_indent(column))(success(Sym::semicolon, "post_end_semicolon")));
}

/**
 * Like `post_end_semicolon`, but for layout end.
 */
Parser repeat_end(uint32_t column) {
  return sym(Sym::end)(iff(cond::smaller_indent(column))(layout_end("repeat_end")));
}

/**
 * To be called after parsing a newline, with the indent of the next line as argument.
 *
 * Checks for eof and end of layout by dedent and `where`.
 */
Parser newline(uint32_t indent) {
  return
    eof +
    with(state::next_char)(comment) +
    dedent(indent) +
    newline_where(indent) +
    newline_semicolon(indent) +
    with(state::next_char)(close_layout_in_list)
    ;
}

/**
 * Parsers that have to run when the next non-space character is not a newline:
 *
 *   - Layout start
 *   - ending nested layouts at the same position
 *   - symbolic operators
 *   - Tokens `where`, `in`, `$`, `)`, `]`, `,`
 *   - comments
 */
Parser immediate(uint32_t column) {
  return
    layout_start(column) +
    post_end_semicolon(column) +
    repeat_end(column) +
    with(state::next_char)(inline_tokens)
    ;
}

/**
 * Parsers that have to run _before_ parsing whitespace:
 *
 *   - Error check
 *   - Indent stack initialization
 *   - Qualified module dot (leading whitespace would mean it would be `(.)`)
 *   - cpp
 */
Parser init = eof + iff(cond::after_error)(fail) + initialize_without_module + dot + cpp_init;

/**
 * The main parser checks whether the first non-space character is a newline and delegates accordingly.
 */
Parser main =
  skipspace +
  eof +
  mark +
  either(
    cond::consume('\n'),
    cpp + with(count_indent)(newline),
    with(state::column)(immediate)
  );

/**
 * The entry point to the parser.
 */
Parser all = init + main;

}

// --------------------------------------------------------------------------------------------------------
// Evaluation
// --------------------------------------------------------------------------------------------------------

namespace eval {

/**
  * Helper that consumes all characters up to the next whitespace, for debugging after a run.
  *
  * Note: This may break the parser, since not all paths use `mark`.
  */
void debug_lookahead(State & state) {
  string s = "";
  for (;;) {
    if (cond::peekws(state) || cond::peekeof(state)) break;
    else {
      s+= state::next_char(state);
      state::advance(state);
    }
  }
  if (!s.empty()) logger("next: " + s);
}

/**
  * The main function of the parsing machinery, executing the parser by passing in the initial state and analyzing the
  * result.
  *
  * If the parser concluded with success, the `result_symbol` attribute of the lexer is set, by which the parsed symbol
  * is communicated to tree-sitter, and `true` is returned, indicating to tree-sitter to use the result.
  *
  * If the parser concluded with failure, no `result_symbol` is set and `false` is returned.
  *
  * If the parser did _not_ conclude, i.e. all steps finished with `cont`, a failure is reported as well.
  *
  * If the `debug_next_token` flag is set, the next token will be printed.
  */
bool eval(logic::Parser chk, State & state) {
  auto result = chk(state);
  if (debug_next_token) debug_lookahead(state);
  if (result.finished && result.sym != Sym::fail) {
    logger("result: " + syms::name(result.sym));
    state.lexer->result_symbol = result.sym;
    return true;
  } else return false;
}

}

// --------------------------------------------------------------------------------------------------------
// API
// --------------------------------------------------------------------------------------------------------

extern "C" {

/**
 * This function allocates the persistent state of the parser that is passed into the other API functions.
 */
void *tree_sitter_haskell_external_scanner_create() { return new vector<vector<uint16_t>>(); }

/**
 * Main logic entry point.
 * Since the state is a singular vector, it can just be cast and used directly.
 */
bool tree_sitter_haskell_external_scanner_scan(void *payload, TSLexer *lexer, const bool *syms) {
  auto *indents = static_cast<vector<vector<uint16_t>> *>(payload);
  auto state = State(lexer, syms, *indents);
  logger(state);
  return eval::eval(logic::all, state);
}

/**
 * Copy the current state to another location for later reuse.
 * This is normally more complex, but since this parser's state constists solely of a vector of integers, it can just be
 * copied.
 */
unsigned tree_sitter_haskell_external_scanner_serialize(void *payload, char *buffer) {
  auto *state = static_cast<vector<vector<uint16_t>> *>(payload);
  size_t i = 0;
  buffer[i++] = state->size();
  for (auto indents : *state) {
    buffer[i++] = indents.size();
    copy(indents.begin(), indents.end(), buffer + i);
    i += indents.size();
  }
  return i;
}

/**
 * Load another parser state into the currently active state.
 * `payload` is the state of the previous parser execution, while `buffer` is the saved state of a different position
 * (e.g. when doing incremental parsing).
 */
void tree_sitter_haskell_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  auto *state = static_cast<vector<vector<uint16_t>> *>(payload);
  state->clear();
  size_t i = 0;
  if (length > 0) {
    auto count = static_cast<size_t>(buffer[i++]);
    for (auto j = 0; j < count; j++) {
      size_t k = buffer[i++];
      vector<uint16_t> indents;
      copy(buffer + i, buffer + i + k, back_inserter(indents));
      state->push_back(indents);
      i += k;
    }
  }
}

/**
 * Destroy the state.
 */
void tree_sitter_haskell_external_scanner_destroy(void *payload) {
  delete static_cast<vector<vector<uint16_t>> *>(payload);
}

}
