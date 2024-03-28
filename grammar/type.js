const {parens, braces} = require('./util.js')

module.exports = {

  // ------------------------------------------------------------------------
  // type
  // ------------------------------------------------------------------------

  type_variable: $ => $._varid,

  annotated_type_variable: $ => seq($.type_variable, $._type_annotation),

  _tyvar: $ => choice(
    parens($, $.annotated_type_variable),
    $.type_variable,
  ),

  _tyvar_in_parens: $ => choice(
    parens($, $._tyvar_in_parens),
    $.annotated_type_variable,
    $.type_variable,
  ),

  inferred_type_variable: $ => braces($, choice($.annotated_type_variable, $.type_variable)),

  _quantifier: $ => choice(
    $._tyvar,
    $.inferred_type_variable,
  ),

  _forall_kw: _ => choice('forall', '∀'),

  _forall_dot: $ => choice('.', $._arrow),

  _forall: $ => seq(
    $._forall_kw,
    repeat($._quantifier),
  ),

  _quantifiers: $ => seq(
    alias($._forall, $.quantifiers),
    $._forall_dot,
  ),

  forall: $ => seq(
    alias($._forall, $.quantifiers),
    $._forall_dot,
  ),

  type_parens: $ => parens($, $._ktype),

  type_list: $ => brackets($, sep1(',', $._ktype)),

  _type_tuple: $ => sep2(',', $._ktype),

  type_tuple: $ => parens($, $._type_tuple),

  _type_sum: $ => sep2('|', optional($._ktype)),

  _type_promotable_literal: $ => alias($.literal, $.type_literal),

  _type_promoted_literal: $ => seq('\'', $._type_promotable_literal),

  _type_literal: $ => choice(
    alias($._type_promoted_literal, $.promoted),
    $._type_promotable_literal,
  ),

  _type_promotable: $ => choice(
    $.type_tuple,
    $.type_list,
  ),

  _type_promoted: $ => seq('\'', $._type_promotable),

  type_name: $ => choice(
    $.type_variable,
    prec('type-name', $._gtycon),
  ),

  type_star: _ => choice('*', '★'),

  type_unboxed_tuple: $ => unboxed_tuple($, $._ktype),

  type_unboxed_sum: $ => unboxed_sum($, $._ktype),

  type_wildcard: _ => '_',

  _atype: $ => choice(
    $.type_name,
    $.type_star,
    $._type_literal,
    $.type_parens,
    alias($._type_promoted, $.promoted),
    $._type_promotable,
    $.type_unboxed_tuple,
    $.type_unboxed_sum,
    $.splice,
    $.quasiquote,
    $.type_wildcard,
  ),

  // Prefix operator lexemes
  type_strict: $ => seq($._any_prefix_bang, $._atype),
  type_lazy: $ => seq($._any_prefix_tilde, $._atype),
  type_invisible: $ => seq($._prefix_at, $._atype),
  modifier: $ => seq($._prefix_percent, $._atype),

  _type_apply_arg: $ => prec.left('apply', choice($._btype, $.type_invisible)),

  /**
   * Type application, as in `Either e (Int, Text)` or `TypeRep @Int`.
   */
  type_apply: $ => prec('apply', seq(
    field('head', $._btype),
    repeat1(field('argument', $._type_apply_arg)),
  )),

  type_infix: $ => prec.right('infix', seq(
    field('left_operand', $._btype),
    field('operator', $._tyops),
    field('right_operand', $._btype),
  )),

  _btype: $ => choice(
    $.type_infix,
    $.type_apply,
    $._atype,
  ),

  type_forall: $ => prec.right('fun', seq($._quantifiers, $._ktype)),

  _fun_arrow: $ => seq(
    optional($._phantom_arrow),
    $._arrow,
  ),

  _linear_fun_arrow: $ => choice(
    seq(
      $.modifier,
      optional($._phantom_arrow),
      $._arrow,
    ),
    $._linear_arrow,
  ),

  type_fun: $ => prec.right('fun', seq($._ktype, $._fun_arrow, $._ktype)),

  type_linear_fun: $ => prec.right('fun', seq($._ktype, $._linear_fun_arrow, $._ktype)),

  _type_annotation: $ => seq(
    $._colon2,
    $._ktype,
  ),

  type_annotated: $ => prec.right('annotated', seq(
    $._ktype,
    $._type_annotation,
  )),

  type_context: $ => prec.right('fun', seq(
    field('context', $._context),
    $._carrow,
    $._ktype,
  )),

  _type_implicit: $ => $.implicit_param,

  _ktype: $ => choice(
    $.type_annotated,
    $.type_fun,
    $.type_linear_fun,
    $.type_forall,
    $.type_context,
    $._type_implicit,
    $._btype,
  ),

  // ------------------------------------------------------------------------
  // type decl
  // ------------------------------------------------------------------------

  decl_type: $ => seq(
    'type',
    $._btype,
    choice(
      seq('=', $._ktype),
      $._type_annotation
    ),
  ),

  // ------------------------------------------------------------------------
  // type instance
  // ------------------------------------------------------------------------

  _type_instance: $ => seq(
    optional($.forall),
    alias($._btype, $.pattern),
    '=',
    $._ktype,
  ),

  decl_tyinst: $ => seq(
    'type',
    'instance',
    $._type_instance,
  ),

  // ------------------------------------------------------------------------
  // type family
  // ------------------------------------------------------------------------

  type_family_result: $ => seq('=', $._ktype),

  type_family_injectivity: $ => seq(optional($._phantom_bar), '|', $.type_variable, $._arrow, repeat1($.type_variable)),

  _tyfam_inj: $ => seq(
    $.type_family_result,
    optional($.type_family_injectivity),
  ),

  _tyfam: $ => seq(
    alias($._btype, $.head),
    optional(choice($._type_annotation, $._tyfam_inj)),
  ),

  _tyfam_equations: $ => layout($, alias($._type_instance, $.equation)),

  _where_equations: $ => seq($._where, optional($._tyfam_equations)),

  decl_tyfam: $ => seq(
    'type',
    'family',
    $._tyfam,
    optional(field('where', $._where_equations)),
  ),

  // ------------------------------------------------------------------------
  // role
  // ------------------------------------------------------------------------

  type_role: _ => choice(
    'representational',
    'nominal',
    'phantom',
    '_',
  ),

  _role: _ => 'role',

  decl_role: $ => seq(
    'type',
    $._role,
    $._tycons,
    repeat1($.type_role),
  )
}
