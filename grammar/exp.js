const {parens, brackets, braces, sep1, layouted, qualified, varid_pattern} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // expression
  // ------------------------------------------------------------------------

  exp_parens: $ => parens($._exp),

  /**
    * This needs to be disambiguated from `gcon_tuple`, which is a constructor with _only_ commas.
    * Tuple sections aren't allowed in patterns.
    *
    * Since tuple expressions can contain singular expressions in sections like `(a,)` and `(,a)`, it has to be ensured
    * that there is _at least_ each one comma and one expression in there, but the comma may be on either side and be
    * preceded by any number of further commas, like `(,,,a)`.
    *
    * The final `repeat` is simpler, it just has to ensure that no two `_exp`s can be successive, but this encoding
    * means that the optional `_exp` after `(5,)` needs to be included in the `choice`, otherwise a simple pair would be
    * impossible.
    */
  _exp_tuple: $ => seq(
    choice(seq(repeat1($.comma), $._exp), seq($._exp, $.comma, optional($._exp))),
    repeat(seq($.comma, optional($._exp)))
  ),

  exp_tuple: $ => parens($._exp_tuple),

  /**
  * Unlike their boxed variants, unboxed tuples may be nullary and unary, making it simpler to parse them.
  * The nullary tuple may even have no space between the hashes, but this format coincides with the prefix notation of
  * the `##` symop. Since the latter is already parsed by other rules and is valid in the same positions, it is left out
  * here.
  *
  * The opening lexeme, `(#`, is parsed with a hardcoded trailing space in exp, pat and type. This is a hack that works
  * around some peculiarities of the interactions with some features like TH and symbolic operators that would most
  * likely be significantly more complex to implement correctly. As it stands, the grammar can't parse an unboxed sum
  * exp without a leading space, as in `(#| x #)`.
  */
  exp_unboxed_tuple: $ => seq($._unboxed_open, sep($.comma, optional($._exp)), $._unboxed_close),

  /**
  * Unboxed sums must have at least one separating `|`, otherwise the expression would be a unary or nullary tuple.
  */
  _exp_unboxed_sum: $ => sep2('|', optional($._exp)),

  exp_unboxed_sum: $ => seq($._unboxed_open, $._exp_unboxed_sum, $._unboxed_close),

  exp_list: $ => brackets(sep1($.comma, $._exp)),

  bind_pattern: $ => seq(
    $._typed_pat,
    $._larrow,
    $._exp,
  ),

  /**
    * An expression like `[1,2..20]`.
    *
    * The two dots are handled in the scanner to disambiguate module and projection dots:
    *
    * - `[A.b]`
    * - `[a.b]`
    *
    * The reason for `choice($._arith_dotdot, '..')` is simply to avoid having to add another case to the scanner.
    * The disambiguation is only performed when the first dot occurs immediately after the identifier, since succeeding
    * whitespace is not allowed for module/projection dots.
    * When the scanner encounters two dots with no further symbolic characters, it rejects the token, deferring to the
    * grammar.
    * We could instead check for the `_arith_dotdot` symbol, but we have to reject the token anyway for record
    * wildcards, so we can just fall back to the grammar for this as well.
    */
  exp_arithmetic_sequence: $ => brackets(
    field('from', $._exp),
    optional(seq($.comma, field('step', $._exp))),
    choice($._arith_dotdot, '..'),
    optional(field('to', $._exp)),
  ),

  /**
   * TransformListComp.
   *
   * These have to be spelled out because the keywords are valid varids when the extension is disabled and it causes
   * errors if they are used individually.
   */
  transform: $ => choice(
    seq('then group by', $._exp, 'using', $._exp),
    seq('then group using', $._exp),
    seq('then', $._exp),
  ),

  qual: $ => choice(
    $.bind_pattern,
    $.let,
    $.transform,
    $._exp,
  ),

  exp_list_comprehension: $ => brackets(
    $._exp,
    '|',
    sep1($.comma, $.qual),
  ),

  exp_section_left: $ => parens(
    $._exp_infix,
    $._qop,
  ),

  exp_section_right: $ => parens(
    $._qop_nominus,
    $._exp_infix,
  ),

  exp_th_quoted_name: $ => choice(
    seq(quote, choice($._qvar, $._qcon)),
    seq(quote + quote, $._atype),
  ),

  exp_field: $ => choice(
    alias('..', $.wildcard),
    seq(
      field('field', $._qvar),
      repeat(seq($._immediate_dot, field('subfield', $._immediate_variable))),
      optional(seq('=', $._exp))
    ),
  ),

  exp_type_application: $ => seq('@', $._atype),

  exp_lambda: $ => seq(
    '\\',
    repeat1($._apat),
    $._arrow,
    $._exp,
  ),

  exp_in: $ => seq('in', $._exp),

  let: $ => seq('let', optional($.decls)),

  _let_decls: $ => layouted_without_end($, $._decl),

  exp_let: $ => seq('let', optional(alias($._let_decls, $.decls))),

  exp_let_in: $ => seq($.exp_let, $.exp_in),

  exp_cond: $ => seq(
    'if',
    field('if', $._exp),
    optional(';'),
    'then',
    field('then', $._exp),
    optional(';'),
    'else',
    field('else', $._exp),
  ),

  pattern_guard: $ => seq(
    $._pat,
    $._larrow,
    $._exp_infix,
  ),

  guard: $ => choice(
    $.pattern_guard,
    $.let,
    $._exp_infix,
  ),

  guards: $ => seq('|', sep1($.comma, $.guard)),

  gdpat: $ => seq($.guards, $._arrow, $._exp),

  exp_if_guard: $ => seq('if', repeat1($.gdpat)),

  _alt_variants: $ => choice(
    seq($._arrow, $._exp),
    repeat1($.gdpat),
  ),

  alt: $ => seq($._pat, $._alt_variants, optional(seq($.where, optional($.decls)))),
  nalt: $ => seq(repeat1($._apat), $._alt_variants, optional(seq($.where, optional($.decls)))),

  alts: $ => layouted($, $.alt),
  nalts: $ => layouted($, $.nalt),

  exp_case: $ => seq('case', $._exp, 'of', optional($.alts)),

  /**
   * left associative because the alts are optional
   */
  exp_lambda_case: $ => seq(
    '\\',
    'case',
    optional($.alts),
  ),

  /**
   * alts are NOT optional in a \cases expression
   */
  exp_lambda_cases: $ => seq(
    '\\',
    'cases',
    $.nalts,
  ),

  rec: $ => seq(
    'rec',
    layouted($, $.stmt),
  ),

  stmt: $ => choice(
    $._exp,
    $.bind_pattern,
    $.let,
    $.rec,
  ),

  _do_keyword: _ => choice('mdo', 'do'),

  do_module: $ => qualified($, $._do_keyword),

  exp_do: $ => seq(choice($.do_module, $._do_keyword), layouted($, $.stmt)),

  exp_negation: $ => seq('-', $._aexp),

  exp_record: $ => seq($._aexp, braces(sep1($.comma, $.exp_field))),

  exp_name: $ => choice(
    $._qvar,
    $._qcon,
    $.implicit_parid,
    $.label,
  ),

  /**
    * Unlike module dot or projection dot, the projection selector dot can match in positions where any varsym can
    * match: `(.name)` vs. `(.::+)`.
    * Furthermore, it can have whitespace between the paren and the dot.
    * Handling this with the dot logic in the scanner would require unreasonable complexity, and since record fields can
    * only be varids, we simply hardcode that here.
    */
  exp_projection_selector: $ => parens(
    '.',
    field('field', $._immediate_variable),
    repeat(seq($._immediate_dot, field('field', $._immediate_variable))),
  ),

  /**
   * The Report lists for `aexp` only expressions that don't have any unbracketed whitespace, except for record
   * construction/update.
   * The GHC parser, however, includes lambdas, let/in and extensions like lambda case in it.
   *
   * Dot-syntax projection works only with simple `aexp`s. For example, these are valid:
   *
   * - `(a <> b).name`
   * - `[a, b].name`
   * - `(,).name`
   * - `[e|a|].name`
   * - `$splice.name`
   * - `Animal {name = "cat"}.name`
   * - `(.name).name`
   * - `(# 1, 2 #).name` (doesn't typecheck, but might in the future?)
   *
   * Some are clear parse errors:
   *
   * - `@Int.name`
   *
   * Others simply don't make sense since they bind the projection into a subexpression, (lambda case and do), even
   * though the grammar works fine if they are included here.
   * We simply keep them out to reduce complexity.
   */
  _aexp_projection: $ => choice(
    $.exp_name,
    $.exp_parens,
    $.exp_tuple,
    $.exp_list,
    $.exp_th_quoted_name,
    $.exp_record,
    $.exp_arithmetic_sequence,
    $.exp_list_comprehension,
    $.exp_section_left,
    $.exp_section_right,
    $.exp_unboxed_tuple,
    $.exp_unboxed_sum,
    $.exp_projection_selector,
    $.splice,
    $.quasiquote,
    alias($.literal, $.exp_literal),
  ),

  /**
    * A dot-syntax field projection like `var.name.othername`.
    * Since fields can only be varids, we can just use `token.immediate` to enforce no whitespace between dot and ids.
    */
  exp_projection: $ => seq(
    choice($._aexp_projection, $.exp_projection),
    $._immediate_dot,
    field('field', $._immediate_variable),
  ),

  _aexp: $ => choice(
    $._aexp_projection,
    $.exp_type_application,
    $.exp_lambda_case,
    $.exp_lambda_cases,
    $.exp_do,
    $.exp_projection,
  ),

  /**
   * Function application.
   *
   * This convoluted rule is necessary because of BlockArguments with lambda – if `exp_lambda` is in `lexp` as is stated
   * in the reference, it can only occur after an infix operator; if it is in `aexp`, it causes lots of problems.
   * Furthermore, the strange way the recursion is done here is to avoid local conflicts.
   */
  _exp_apply: $ => choice(
    $._aexp,
    seq($._aexp, $._exp_apply),
    seq($._aexp, $.exp_lambda),
    seq($._aexp, $.exp_let_in),
    seq($._aexp, $.exp_cond),
    seq($._aexp, $.exp_case),
  ),

  /**
   * The point of this `choice` is to get a node for function application only if there is more than one expression
   * present.
   */
  _fexp: $ => choice(
    $._aexp,
    alias($._exp_apply, $.exp_apply),
  ),

  _lexp: $ => choice(
    $.exp_let_in,
    $.exp_cond,
    $.exp_if_guard,
    $.exp_case,
    $.exp_negation,
    $._fexp,
    $.exp_lambda,
  ),

  /**
   * This is left-associative, although in reality this would depend on the fixity declaration for the operator.
   * The default is left, even though the reference specifies it the other way around.
   * In any case, this seems to be more stable.
   */
  exp_infix: $ => seq($._exp_infix, $._qop, $._lexp),

  _exp_infix: $ => choice(
    $.exp_infix,
    $._lexp,
  ),

  /**
   * `prec.right` because:
   *
   * let x = 1 in x :: Int
   *
   * here the type annotation binds to `x`, not the entire expression
   */
  _exp: $ => prec.right(seq($._exp_infix, optional($._type_annotation))),
}
