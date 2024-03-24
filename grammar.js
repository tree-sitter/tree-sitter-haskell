const id = require('./grammar/id.js')
const operator = require('./grammar/operator.js')
const lexeme = require('./grammar/lexeme.js')
const type = require('./grammar/type.js')
const context = require('./grammar/context.js')
const exp = require('./grammar/exp.js')
const pat = require('./grammar/pat.js')
const import_ = require('./grammar/import.js')
const module_ = require('./grammar/module.js')
const data = require('./grammar/data.js')
const class_ = require('./grammar/class.js')
const decl = require('./grammar/decl.js')
const literal = require('./grammar/literal.js')
const patsyn = require('./grammar/patsyn.js')
const th = require('./grammar/th.js')

module.exports = grammar({
  name: 'haskell',

  /**
   * These rules may occur anywhere in the grammar and don't have to be specified in productions.
   */
  extras: $ => [
    /\p{Zs}/,
    /\n/,
    /\r/,
    $.cpp,
    $.comment,
    $.haddock,
    $.pragma,
  ],

  /**
   * These rules are handled manually by the custom lexer in `src/scanner.c`.
   * Whenever the symbols are used in the rule tree, the parser executes the scanner.
   * Since several symbols are present both here and in `extras`, the scanner will be invoked before every single
   * natively lexed symbol, repeatedly, until it fails.
   * This makes indentation/layout tracking simpler.
   *
   * There are three special behavioral classes of symbols in this grammar, indicated by their name prefix:
   *
   * `cmd`: Commands are sequenced after key tokens like opening braces that unconditionally trigger a state change in
   *        the scanner.
   *        They don't require the scanner to _decide_ that a symbol is valid – the native parser has already committed
   *        to a symbol, and the command relays that information to the scanner so it can update its state and return.
   *        For example, after having parsed an opening brace, the scanner adds a brace context to the stack in reaction
   *        to `_cond_brace_open`, which will influence its future behavior.
   *        These symbols do not produce nodes in the parse tree.
   *
   * `cond`: Conditions are hard requirements in the grammar that are decided by the scanner based on its state and the
   *         lookahead.
   *         For example, whitespace-sensitive operators (tight infix /prefix ops) like the `@` in a type application or
   *         the `.` in a record projection are lexed when the following characters have certain properties.
   *         When the scanner decides that such a symbol is valid, the parser cannot attempt to use an alternative
   *         interpretation like it can with conflicts.
   *         This is often difficult, but also especially useful to force declarations and other layout items to be
   *         terminated when a new line has smaller or equal indent, preventing incorrect interpretations as top level
   *         expression splices spanning multiple lines.
   *
   * `phantom`: Phantom symbols are used to signal to the scanner that certain constructs are valid without requiring
   *            the scanner to actually parse them and produce text nodes for them.
   *            In the grammar, they are always optional, and the scanner never emits the phantom symbol itself, but
   *            decides whether to emit _other_ symbols when the phantom symbol is _not_ valid.
   *            This is used to implement GHC's tactic of allowing layouts to end _whenever a parse error occurs_, but
   *            using specialized heuristics.
   *
   * Most other symbols produce nodes, except for newline and the error sentinel.
   */
  externals: $ => [

    // This is an unused symbol that indicates to the scanner that a parse error has occurred.
    // Tree-sitter marks _all_ symbols as valid when calling the scanner after an error, so a symbol that's not used in
    // the grammar is only valid if that is the case.
    $.error_sentinel,

    // Emitted after every newline with equal or less indent to terminate layout-based rules, with a few exceptions.
    $._cond_layout_semicolon,

    // Instruct the scanner to push a layout context.
    // The first one is for declarations and miscellaneous other layouts.
    // The others are for the specific syntax construct.
    // The quote layout is for declaration quotes (`[d|data A ...|]`).
    $._cmd_layout_start,
    $._cmd_layout_start_do,
    $._cmd_layout_start_case,
    $._cmd_layout_start_if,
    $._cmd_layout_start_let,
    $._cmd_layout_start_quote,

    // Emitted when a new line's indent mandates ending the current layout (depending on the layout sort), or when a
    // special inline layout-ending token is encountered, like an `in`.
    $._cond_layout_end,

    // Instruct the scanner to push or pop a brace context.
    $._cmd_brace_open,
    $._cmd_brace_close,

    // Instruct the scanner to push or pop a tuple expression context.
    // In parenthesized or bracketed expressions, certain tokens (like commas, bars, and closing brackets), can end
    // layouts, so the scanner must be aware of them.
    $._cmd_texp_start,
    $._cmd_texp_end,

    // Signal to the scanner that these symbols are valid.
    // See the explanation of phantom symbols above.
    $._phantom_where,
    $._phantom_in,
    $._phantom_arrow,
    $._phantom_bar,
    $._phantom_deriving,

    // Detect and emit text nodes for comments and CPP.
    // In particular, #else branches of CPP conditionals a fully contained in the resulting node, since their nonlinear
    // nature means they cannot be parsed.
    $.comment,
    $.haddock,
    $.cpp,
    $.pragma,

    // Starting quote brackets are a bit messy, so it's easier to let the scanner signal that it encountered one.
    // The body produces a text node until the ending bracket.
    $._cond_quote_start,
    $.quasiquote_body,

    // Whitespace-sensitive operators for splices (`$`, `$$`), projection and module dots (`a.b`, `A.b`), arithmetic
    // sequence dots `[1..10]`, as-pattern (`a@(Just b)`), type application (`@Int`), strictness and laziness
    // annotations (`!pat`, `~Int`), and modifiers (`Int %1 -> Int`).
    $._cond_splice,
    $._cond_qual_dot,
    $._cond_tight_dot,
    $._cond_prefix_dot,
    $._cond_dotdot,
    $._cond_tight_at,
    $._cond_prefix_at,
    $._cond_tight_bang,
    $._cond_prefix_bang,
    $._cond_tight_tilde,
    $._cond_prefix_tilde,
    $._cond_prefix_percent,

    // Symbolic operators, producing text nodes.
    // These are very difficult to parse in the grammar, because unlike most languages, Haskell's operators are not a
    // finite set, and therefore cannot be lexically disambiguated from special operators like `->`.
    // In the presence of runtime conflicts, this can easily lead to the invalid interpretation of reserved operators as
    // identifers.
    // Furthermore, the regexes for all the unicode categories produce very large ternary operator trees if specified
    // in the grammar.
    $._varsym,
    $._consym,

    // The newline is used as a dummy symbol that is emitted whenever the scanner has to update its state even though
    // the current position must be parsed by the grammar.
    /\n/,

  ],

  inline: $ => [
    $._number,
    $._stringly,
    $._literal,

    $._var,
    $._vars,
    $._varids,
    $._varids_ticked,
    $._varops,

    $._varsyms,
    $._varop,

    $._modid,

    $._conids,
    $._con,
    $._qcon,
    $._cons,
    $._conop,
    $._conids_ticked,
    $._conops,

    $._qconsym,
    $._consyms,

    $._op_ticked,

    $._tyconid,
    $._tyconids,
    $._tycon,
    $._qtycon,
    $._tycons,
    $._tyconops,
    $._tyops,

    $._sym,
    $._qsym,
    $._qsym_prefix,

    $._gtycon,
    $._tyvar,
    $._quantifiers,
    $._type_annotation,

    $._con_btype,

    $._exp_apply_arg,
    $._pat_apply_arg,

    $._tyvar_binder,
    $._type_head_binder,
    $._type_head_binders,

    $._atype,

    // ------------------------------------------------
    // context
    // ------------------------------------------------

    $._constraint,

    // ------------------------------------------------
    // dot
    // ------------------------------------------------

    $._any_prefix_dot,

    // ------------------------------------------------
    // function vs bind
    // ------------------------------------------------

    $._funlhs,
    $._funlhs_parens,
    $._funvar,

  ],

  precedences: $ => [

    // ------------------------------------------------
    // expressions, patterns and types
    // ------------------------------------------------

    [
      'projection',
      'record',
      'prefix',
      'apply',
      'negation-reduce',
      'infix-qualified',
      'infix-minus',
      'infix',
      'fun',
      'implicit',
      'annotated',
      'view',
      $._ktype,
    ],

    /**
     * `f A a a = a`
     *
     * This should not be parsed as constructor application, but separate patterns.
     */
    [
      'patterns',
      'apply',
    ],

    /**
     * `data A = A Int Int`
     *
     * The `Int Int` could be associated more strongly to produce application.
     * Same for infix.
     */
    [
      'datacon-param',
      'apply',
      'infix',
    ],

    // ------------------------------------------------
    // negation
    // ------------------------------------------------

    [
      'operator-minus',
      'negation',
      'section-minus-shift',
    ],

    // ------------------------------------------------
    // patsyn
    // ------------------------------------------------

    [
      'patsyn',
      'qcon',
    ],

    // ------------------------------------------------
    // function vs bind
    // ------------------------------------------------

    [
      'function-nullary',
      'pat-name',
    ],

    // ------------------------------------------------
    // qualified names
    // ------------------------------------------------

    [
      'qualifying-module',
      'qualified-id',
    ],

    [
      'qualifying-module',
      'promoted-tycon',
    ],

    [
      'qualifying-module',
      'qcon',
    ],

    [
      'qualifying-module',
      'type-name',
    ],

    // ------------------------------------------------
    // type/context
    // ------------------------------------------------

    [
      $.operator,
      $.type_star,
    ],

    // ------------------------------------------------
    // misc
    // ------------------------------------------------

    [
      $._transform_group,
      $._variable_group,
    ],

  ],

  conflicts: $ => [

    /**
     * Contexts and classes could reuse the rules for types, but then it would be up to queries to distinguish class
     * names based on parent nodes.
     * This would be tedious and incomplete, since those structures can nest arbitrarily.
     */
    [$.type_apply, $.class_apply],
    [$.type_infix, $.class_infix],
    [$._constraint_implicit, $._type_implicit],
    [$._btype, $._aclass],

    /**
     * Application of tycons doesn't suffer from the same interference as expressions and patterns (in particular prefix
     * minus), so it can get a flat list of arguments rather than left-associative nested nodes.
     */
    [$.type_apply],

    /**
     * For reference in GHC:
     * - Note [Ambiguous syntactic categories]
     * - Note [PatBuilder]
     * - Note [Declaration/signature overlap]
     * - These correspond to `DisambECP`
     *
     * (fun x) y = undefined
     * (fun x -> y) = undefined
     * (fun) <> x = undefined
     *
     * The first one is a regular function with some redundant parens, where `fun` is the declared name.
     * The second one is a pattern binder with a view pattern, where `fun` is a free variable.
     * The third one is an infix pattern binder, where `fun` is a simple varid pattern with redundant parens.
     *
     * These conflicts are also relevant for top-level expression splices, which fundamentally conflict with decls, and
     * since decls start with either `var` or `pat`, they cannot be disambiguated.
     *
     * GHC parses functions and binds as expressions and sorts them into the right LHS in a post-processing step.
     * Since this is not possible in tree-sitter, these conflicts are more function-centric than in GHC.
     *
     * function variable:
     * func (A a) = a
     *
     * function infix:
     * a : as = [1, 2, 3]
     *
     * bind:
     * Just 1 = Just 1
     *
     * splice:
     * makeLenses ''A
     *
     * Signature and pattern binder:
     *
     * fun :: Int
     * fun :: Int = 5
     */

    // Function vs bind vs splice
    [$._fun_name, $.pat_name],
    [$._fun_name, $.exp_name],
    [$._fun_name, $.exp_name, $.pat_name],

    // Signature vs bind
    [$.signature, $.pat_name],

    // Splice vs bind
    [$._infixexp, $._infixpat],
    [$.exp_name, $.pat_name],
    [$.literal, $.pat_negation],

    /**
     * types conflicting with structures that look like types
     *
     * `type` and `constructor` use the same terminal symbol, but we don't want to use the "internal" term `conid` in
     * the grammar, but rather those more semantic terms, in order to be accessible.
     * Therefore, `type` has to be aliased at the lowest level since tree-sitter doesn't have the capability for
     * post-processing parse trees in the grammar.
     *
     * In GHC, this corresponds to `DisambTD`.
     *
     * data A = Name Int
     * data A = Maybe Int :+ Int
     * data A = Monoid a => A a
     *
     * All of these start with a `constructor` followed by a `type_name`, but for the last two interpretations, the
     * `constructor` has to be reduced to `_btype` before shifting over the `type_name`, while it isn't reduced at all
     * for the data constructor.
     *
     * In GHC, these correspond to `mkHsAppTyHeadPV` and `mkHsAppTyPV`.
     *
     * data A a b = a `C` b => a `A` b
     * data A a b = a `A` b
     *
     * data a *** b
     * data a +++ b => a *** b
     *
     * The second one is already handled by the more general type/context conflicts above.
     * In GHC, this corresponds to `mkHsOpTyPV`.
     */
    [$.type_name, $.data_constructor],
    [$._constructor_ticked, $._tycon_ticked],

    // ------------------------------------------------------------------------
    // unboxed data
    // ------------------------------------------------------------------------

    /**
     * The hash in the opening parenthesis of unboxed tuples can be an operator.
     */
    [$.operator, $._unboxed_open],

    /**
     * `(# ... | ...` exists in both `exp` and `pat`.
     */
    [$.pat_unboxed_sum, $.exp_unboxed_sum],

    /**
     * `(# ... , ...` exists in both `exp` and `pat`.
     */
    [$.exp_unboxed_tuple, $.pat_unboxed_tuple],

  ],

  word: $ => $._varid,

  rules: {
    haskell: $ => seq(
      optional($.header),
      optional($._body),
    ),

    _topdecl: $ => choice(
      $._decl,
      alias($.decl_type, $.type_alias),
      alias($.decl_tyfam, $.type_family),
      alias($.decl_tyinst, $.type_instance),
      alias($.decl_role, $.role_annotation),
      alias($.decl_data, $.data_type),
      alias($.decl_newtype, $.newtype),
      alias($.decl_datafam, $.data_family),
      alias($.decl_datainst, $.data_instance),
      alias($.decl_class, $.class),
      alias($.decl_instance, $.instance),
      alias($.decl_default, $.default_declaration),
      $._decl_foreign,
      alias($.decl_deriving, $.deriving_declaration),
      alias($.decl_patsyn, $.pattern_synonym),
      $.top_splice,
    ),

    ...type,
    ...context,
    ...exp,
    ...pat,
    ...import_,
    ...module_,
    ...data,
    ...class_,
    ...decl,
    ...patsyn,
    ...th,
    ...literal,
    ...id,
    ...operator,
    ...lexeme,

  },

})
