const {parens, brackets, sep1, layout, qualified} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // expression
  // ------------------------------------------------------------------------

  exp_parens: $ => parens($, $._exp),

  /**
    * This needs to be disambiguated from `gcon_tuple`, which is a constructor with _only_ commas.
    * Tuple sections aren't allowed in patterns.
    *
    * Since tuple expressions can contain singular expressions in sections like `(a,)` and `(,a)`, it has to be ensured
    * that there is _at least_ each one comma and one expression in there, but the comma may be on either side and be
    * preceded by any number of further commas, like `(,,,a)`.
    *
    * The final `repeat` is simpler, it just has to ensure that no two `_infixexp`s can be successive, but this encoding
    * means that the optional `_exp` after `(5,)` needs to be included in the `choice`, otherwise a simple pair
    * would be impossible.
    */
  _exp_tuple: $ => seq(
    choice(
      seq(repeat1(','), $._exp),
      seq($._exp, ',', optional($._exp)),
    ),
    repeat(seq(',', optional($._exp)))
  ),

  exp_tuple: $ => parens($, $._exp_tuple),

  /**
   * Unlike their boxed variants, unboxed tuples may be nullary and unary, making it simpler to parse them.
   */
  exp_unboxed_tuple: $ => unboxed_tuple($, $._exp),

  /**
   * Unboxed sums must have at least one separating `|`, otherwise the expression would be a unary or nullary tuple.
   * This is a lenient parser that allows multiple variants to be filled in, for simplicity.
   */
  exp_unboxed_sum: $ => unboxed_sum($, $._exp),

  exp_section_left: $ => parens(
    $,
    $._infixexp,
    choice(
      $._ops,
      $._operator_minus,
    ),
  ),

  exp_section_right: $ => parens(
    $,
    choice(
      prec('section-minus-shift', seq(alias('-', $.operator), $._infixexp)),
      seq(alias($._operator_qual_dot_head, $.operator), $._infixexp),
      seq($._ops, $._infixexp),
    )
  ),

  exp_list: $ => brackets($, sep1(',', $._exp), optional(',')),

  bind_statement: $ => seq(
    $._pat,
    $._larrow,
    $._exp,
  ),

  /**
    * An expression like `[1,2..20]`.
    */
  exp_arithmetic_sequence: $ => brackets(
    $,
    field('from', $._exp),
    optional(seq(',', field('step', $._exp))),
    $._dotdot,
    optional(field('to', $._exp)),
  ),

  _transform_group: $ => $._group,

  /**
   * `TransformListComp`
   * The `group` conflicts, and we solve it the same way GHC does – by statically preferring `transform` over
   * `variable`.
   */
  transform: $ => choice(
    seq('then', $._transform_group, 'by', $._exp, 'using', $._exp),
    seq('then', $._transform_group, 'using', $._exp),
    seq('then', $._exp),
  ),

  _qualifier: $ => choice(
    alias($.bind_statement, $.generator),
    $.let,
    $.transform,
    alias($._exp, $.boolean_guard),
  ),

  qualifiers: $ => seq(sep1(',', field('qualifier', $._qualifier))),

  exp_list_comprehension: $ => brackets(
    $,
    $._exp,
    repeat1(seq('|', $.qualifiers)),
  ),

  exp_th_quoted_name: $ => choice(
    seq('\'', $._cons),
    seq('\'', token.immediate('\''), $._atype),
    seq('\'', $._vars),
  ),

  exp_field: $ => choice(
    alias('..', $.wildcard),
    seq(
      field('field', $._vars),
      repeat(seq($._tight_dot, field('subfield', $.variable))),
      optional(seq('=', $._exp))
    ),
  ),

  exp_type_application: $ => seq($._prefix_at, $._atype),

  exp_lambda: $ => seq(
    '\\',
    $.patterns,
    $._arrow,
    $._exp,
  ),

  in: $ => seq(optional($._phantom_in), 'in', $._exp),

  _let_binds: $ => layout_sort($, $._cmd_layout_start_let, $._decl),

  /**
   * This is not prefixed with `exp` because `let` cannot occur in expression position without `in`, only as guards and
   * do statements.
   *
   * This used to have an optional layout end to simplify inline use.
   * The current approach is to end layouts on tokens like `->` or `=` after guards, unifying with texp layout ends.
   */
  let: $ => seq('let', optional(alias($._let_binds, $.binds))),

  exp_let_in: $ => seq($.let, $.in),

  exp_cond: $ => seq(
    'if',
    field('if', $._exp),
    repeat(';'),
    'then',
    field('then', $._exp),
    repeat(';'),
    'else',
    field('else', $._exp),
  ),

  _guard: $ => choice(
    alias($.bind_statement, $.pattern_guard),
    $.let,
    alias($._exp, $.boolean_guard),
  ),

  guards: $ => sep1(',', field('guard', $._guard)),

  /**
   * Reused by `function`
   */
  _guards: $ => seq(
    optional($._phantom_bar),
    '|',
    $._cmd_texp_start,
    field('guards', $.guards),
  ),

  match: $ => seq(
    field('guards', $._guards),
    optional($._phantom_arrow),
    $._arrow,
    $._cmd_texp_end,
    field('body', $._exp),
  ),

  exp_multi_way_if: $ => seq('if', $._cmd_layout_start_if, repeat($.match), $._cond_layout_end),

  _simple_match: $ => seq($._arrow, field('body', $._exp)),

  _matches: $ => field('match', choice(
    alias($._simple_match, $.match),
    repeat1($.match),
  )),

  alt: $ => seq(
    field('pattern', $._pat),
    $._matches,
    optional($._where_binds),
  ),

  _nalt: $ => seq(
    field('patterns', $.patterns),
    $._matches,
    optional($._where_binds),
  ),

  alts: $ => layout_sort($, $._cmd_layout_start_case, field('alt', $.alt)),
  _nalts: $ => layout_sort($, $._cmd_layout_start_case, field('alt', alias($._nalt, $.alt))),

  exp_case: $ => seq('case', $._exp, 'of', optional($.alts)),

  exp_lambda_case: $ => seq(
    '\\',
    'case',
    optional(field('alts', $.alts)),
  ),

  /**
   * alts are not optional in a `\cases` expression, but we're lenient.
   */
  exp_lambda_cases: $ => seq(
    '\\',
    'cases',
    optional(field('alts', alias($._nalts, $.alts))),
  ),

  rec: $ => seq(
    'rec',
    layout($, $._statement),
  ),

  _statement: $ => choice(
    alias($._exp, $.exp_statement),
    $.bind_statement,
    $.let,
    $.rec,
  ),

  _do_keyword: _ => choice('mdo', 'do'),

  do_module: $ => qualified($, $._do_keyword),

  _do: $ => choice(
    $.do_module,
    $._do_keyword
  ),

  exp_do: $ => seq($._do, layout_sort($, $._cmd_layout_start_do, $._statement)),

  exp_record: $ => prec('record', seq($._infixexp, braces($, sep1(',', $.exp_field)))),

  exp_name: $ => choice(
    $._cons,
    $._vars,
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
    $,
    $._any_prefix_dot,
    field('field', $.variable),
    repeat(seq($._tight_dot, field('field', $.variable))),
  ),

  /**
    * A dot-syntax field projection like `var.name.othername`.
    * Since fields can only be varids, we can just use `token.immediate` to enforce no whitespace between dot and ids.
    */
  exp_projection: $ => seq(
    prec('projection', seq(
      $._infixexp,
      $._tight_dot,
    )),
    field('field', $.variable),
  ),

  /**
   * These block arguments don't end in a layout, so they all range over all following block arguments and will
   * therefore always be the last argument in an application or infix chain.
   * They also pull a trailing type annotation into their body.
   */
  _exp_greedy: $ => choice(
    $.exp_lambda,
    $.exp_let_in,
    $.exp_cond,
  ),

  _exp_apply_arg: $ => choice(
    $._infixexp,
    $.exp_type_application,
  ),

  exp_apply: $ => prec.left('apply', seq(
    $._infixexp,
    field('arg', $._exp_apply_arg),
  )),

  exp_negation: $ => prec('negation', seq($._negation, prec('negation-reduce', $._infixexp))),

  _infix_minus: $ => prec('operator-minus', alias('-', $.operator)),

  _exp_op: $ => choice(
    $._sym,
    $._op_ticked,
    alias($._prefix_dot, $.operator),
  ),

  exp_infix: $ => choice(
    prec.left('infix', seq(
      field('left_operand', $._infixexp),
      field('operator', $._exp_op),
      field('right_operand', $._infixexp),
    )),
    seq(
      field('left_operand', $._infixexp),
      field('operator', $._infix_minus),
      prec('infix-minus', field('right_operand', $._infixexp)),
    ),
    seq(
      field('left_operand', $._infixexp),
      field('operator', $._qsym),
      prec('infix-qualified', field('right_operand', $._infixexp)),
    ),
  ),

  _infixexp: $ => choice(
    $.exp_infix,
    $.exp_negation,
    $.exp_apply,
    $.exp_record,
    $.exp_projection,
    $.exp_arithmetic_sequence,
    $.exp_list_comprehension,
    $.exp_unboxed_tuple,
    $.exp_unboxed_sum,
    $.exp_projection_selector,
    $.quasiquote,
    $.quote,
    $.typed_quote,
    alias($.literal, $.exp_literal),
    $.exp_th_quoted_name,
    $.exp_lambda_case,
    $.exp_lambda_cases,
    $.exp_do,
    $.splice,
    $.exp_parens,
    $.exp_tuple,
    $.exp_list,
    $.exp_section_left,
    $.exp_section_right,
    $._exp_greedy,
    $.exp_case,
    $.exp_multi_way_if,
    $.exp_name,
  ),

  exp_annotated: $ => prec.right('annotated', seq(
    $._infixexp,
    $._type_annotation,
  )),

  _exp: $ => choice(
    $.exp_annotated,
    // Right-associative means that the reduction of `_infixexp` to `_exp` loses against any shift.
    prec.right($._infixexp),
  ),

}
