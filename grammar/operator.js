const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // var
  // ------------------------------------------------------------------------

  _operator_qual_dot_head: $ => seq($._cond_qual_dot, $._varsym),

  operator: $ => choice(
    seq(optional($._cond_prefix_dot), $._varsym),
    '#',
    token.immediate('#'),
    seq(token.immediate('#'), choice(token.immediate('#'), token.immediate('|'))),
    seq('#', token.immediate('#')),
    '*',
  ),

  _operator_alias: $ => $.operator,

  _operator_minus: $ => prec('operator-minus', alias('-', $.operator)),

  _varsym_prefix: $ => parens(
    $,
    choice(
      $.operator,
      $._operator_minus,
      alias($._operator_qual_dot_head, $.operator),
    ),
  ),

  qualified_operator: $ => qualified($, choice($.operator, $._operator_minus)),

  _varsyms: $ => choice($.operator, $.qualified_operator),

  _qualified_operator_prefix: $ => parens($, $.qualified_operator),

  // ------------------------------------------------------------------------
  // con
  // ------------------------------------------------------------------------

  constructor_operator: $ => $._consym,

  _constructor_operator_alias: $ => $.constructor_operator,

  _consym_prefix: $ => parens($, $.constructor_operator),

  _qualified_consym: $ => qualified($, $.constructor_operator),
  _qconsym: $ => alias($._qualified_consym, $.qualified_operator),

  _consyms: $ => choice($.constructor_operator, $._qconsym),

  _qualified_constructor_operator_prefix: $ => parens($, $._qconsym),

  // ------------------------------------------------------------------------
  // op
  // ------------------------------------------------------------------------

  _sym: $ => choice($._operator_alias, $._constructor_operator_alias),

  _qsym_prefix: $ => choice($._qualified_operator_prefix, $._qualified_constructor_operator_prefix),

  _qsym: $ => choice($.qualified_operator, $._qconsym),

}
