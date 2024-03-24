const {parens, brackets, braces} = require('./util.js')

module.exports = {

  pat_field: $ => choice(
    alias('..', $.wildcard),
    seq($._vars, optional(seq('=', $._pat_texp))),
  ),

  pat_fields: $ => braces($, optional(sep1(',', $.pat_field))),

  pat_name: $ => choice(prec('pat-name', $._var), $._cons),

  pat_as: $ => prec('prefix', seq(field('bind', $.variable), $._tight_at, field('pattern', $._infixpat))),

  pat_record: $ => seq(field('constructor', $.pat_name), field('fields', $.pat_fields)),

  pat_wildcard: _ => '_',

  pat_parens: $ => parens($, $._pat_texp),

  pat_tuple: $ => parens($, sep2(',', $._pat_texp)),

  pat_unboxed_tuple: $ => unboxed_tuple($, $._pat_texp),

  pat_unboxed_sum: $ => unboxed_sum($, $._pat_texp),

  pat_list: $ => brackets($, sep1(',', $._pat_texp)),

  pat_strict: $ => prec('prefix', seq($._any_prefix_bang, $._infixpat)),

  pat_irrefutable: $ => prec('prefix', seq($._any_prefix_tilde, $._infixpat)),

  pat_type_binder: $ => seq($._prefix_at, $._atype),

  // -------------------------------------------------------------------------------------------------------------------
  // non-atomic
  // -------------------------------------------------------------------------------------------------------------------

  _pat_apply_arg: $ => choice(
    $._infixpat,
    $.pat_type_binder,
  ),

  pat_apply: $ => prec.left('apply', seq(
    $._infixpat,
    field('arg', $._pat_apply_arg),
  )),

  pat_negation: $ => prec('negation', seq($._negation, $._number)),

  _pat_op: $ => choice(
    $.constructor_operator,
    $._conids_ticked,
  ),

  pat_infix: $ => choice(
    prec.left('infix', seq(
      field('left_operand', $._infixpat),
      field('operator', $._pat_op),
      field('right_operand', $._infixpat),
    )),
    seq(
      field('left_operand', $._infixpat),
      field('operator', $._qconsym),
      prec('infix-qualified', field('right_operand', $._infixpat)),
    ),
  ),

  _infixpat: $ => choice(
    $.pat_infix,
    $.pat_negation,
    $.pat_apply,
    $.pat_name,
    $.pat_as,
    $.pat_record,
    alias($.literal, $.pat_literal),
    $.pat_wildcard,
    $.pat_parens,
    $.pat_tuple,
    $.pat_unboxed_tuple,
    $.pat_unboxed_sum,
    $.pat_list,
    $.pat_strict,
    $.pat_irrefutable,
    $.splice,
    $.quasiquote,
  ),

  pat_annotated: $ => prec.right('annotated', seq(
    field('pattern', $._infixpat),
    $._type_annotation,
  )),

  _pat: $ => choice(
    $.pat_annotated,
    prec.right($._infixpat),
  ),

  patterns: $ => repeat1(prec('patterns', $._pat_apply_arg)),

  pat_view: $ => prec.right('view', seq($._exp, $._arrow, $._pat_texp)),

  _pat_texp: $ => choice(
    $.pat_view,
    $._pat,
  )

}
