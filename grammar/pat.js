const {parens} = require('./util.js')

module.exports = {
  fpat: $ => choice(
    $.dotdot,
    seq($._qvar, optional(seq($.equals, $._pat))),
  ),

  /**
   * { [_qvar [ = _pat] [, ...] ] | .. }
   */
  pat_record: $ => braces(optional(sep1($.comma, $.fpat))),

  pat_as: $ => seq(field('var', $.varid), $.as_pat, field('pat', $._apat)),

  pat_parens: $ => parens($._pat, optional($._type_annotation)),

  pat_view: $ => parens($._exp, $.arrow, $._pat, optional($._type_annotation)),

  pat_tuple: $ => parens(sep2($.comma, $._pat)),

  pat_list: $ => brackets(sep1($.comma, $._pat)),

  pat_strict: $ => seq($.strict, $._apat),

  pat_irrefutable: $ => seq('~', $._apat),

  pat_neg: $ => seq('-', choice($.integer, $.float)),

  pat_name: $ => $.varid,

  pat_con: $ => $._qcon,

  _apat: $ => choice(
    $.pat_name,
    $.pat_as,
    seq(alias($.pat_con, $.pat_name), optional($.pat_record)),
    alias($.literal, $.pat_literal),
    alias($.wildcard, $.pat_wildcard),
    $.pat_parens,
    $.pat_view,
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
}
