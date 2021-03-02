const {parens} = require('./util.js')

module.exports = {
  fpat: $ => choice(
    alias($._dotdot, $.wildcard),
    seq($._qvar, optional(seq($.equals, $._pat))),
  ),

  /**
   * { [_qvar [ = _pat] [, ...] ] | .. }
   */
  pat_record: $ => braces(optional(sep1($.comma, $.fpat))),

  pat_as: $ => seq(field('var', $.variable), $.as_pat, field('pat', $._apat)),

  pat_parens: $ => parens($._nested_pat, optional($._type_annotation)),

  pat_view: $ => seq($._exp, $._arrow, $._nested_pat),

  pat_tuple: $ => parens(sep2($.comma, $._nested_pat)),

  pat_list: $ => brackets(sep1($.comma, $._nested_pat)),

  pat_strict: $ => seq($._strict, $._apat),

  pat_irrefutable: $ => seq('~', $._apat),

  pat_neg: $ => seq('-', choice($.integer, $.float)),

  pat_name: $ => $.variable,

  pat_con: $ => $._qcon,

  _apat: $ => choice(
    $.pat_name,
    $.pat_as,
    seq(alias($.pat_con, $.pat_name), optional($.pat_record)),
    alias($.literal, $.pat_literal),
    alias($._wildcard, $.pat_wildcard),
    $.pat_parens,
    $.pat_tuple,
    $.pat_list,
    $.pat_strict,
    $.pat_irrefutable,
  ),

  /**
   * In patterns, application is only legal if the first element is a con.
   */
  pat_apply: $ => seq(alias($.pat_con, $.pat_name), repeat1($._apat)),

  _lpat: $ => choice(
    $._apat,
    $.pat_neg,
    $.pat_apply,
  ),

  pat_infix: $ => seq($._lpat, $._qconop, $._pat),

  /**
   * Without the precs, a conflict is needed.
   */
  _pat: $ => choice(
    prec(2, $.pat_infix),
    prec(1, $._lpat),
  ),

  /**
   *
   */
  _nested_pat: $ => choice(
    $._pat,
    $.pat_view,
  )
}
