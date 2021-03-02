const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // var
  // ------------------------------------------------------------------------

  _varid: _ => /[_a-z](\w|')*#?/,
  varid: $ => $._varid,
  qvarid: $ => qualified($, $.varid),
  _qvarid: $ => choice($.qvarid, $.varid),

  varsym: $ => $._varsym,
  qvarsym: $ => qualified($, $.varsym),
  _qvarsym: $ => choice($.qvarsym, $.varsym),

  _var: $ => choice($.varid, parens($.varsym)),

  _qvar: $ => choice($._qvarid, parens($._qvarsym)),

  varop: $ => choice($.varsym, ticked($.varid)),

  qvarop: $ => choice($._qvarsym, ticked($._qvarid)),

  implicit_parid: _ => /\?[_a-z](\w|')*/,

  // ------------------------------------------------------------------------
  // con
  // ------------------------------------------------------------------------

  _conid: _ => /[A-Z](\w|')*#?/,
  constructor: $ => $._conid,
  qualified_constructor: $ => qualified($, $.constructor),
  _qconid: $ => choice($.qualified_constructor, $.constructor),

  consym: $ => $._consym,
  qconsym: $ => qualified($, $.consym),
  _qconsym: $ => choice($.qconsym, $.consym),

  _con: $ => choice($.constructor, parens($.consym)),
  _qcon: $ => choice($._qconid, parens($._qconsym)),
  _conop: $ => choice($.consym, ticked($.constructor)),
  _qconop: $ => choice($._qconsym, ticked($._qconid)),
  _op: $ => choice($.varop, $._conop),
  _qop: $ => choice($.qvarop, $._qconop),

  _gcon_literal: $ => choice(
    $.con_unit,
    $.con_list,
    $.con_tuple,
  ),

  literal: $ => choice(
    $._literal,
    $._gcon_literal,
  ),

  _gcon: $ => choice(
    $._qcon,
    $._gcon_literal,
  ),

  // ------------------------------------------------------------------------
  // tycon
  // ------------------------------------------------------------------------

  _tycon: $ => alias($.constructor, $.type),
  qualified_type: $ => qualified($, $._tycon),
  _qtycon: $ => choice($.qualified_type, $._tycon),

  type_operator: $ => $._tyconsym,
  qualified_type_operator: $ => qualified($, $.type_operator),
  _qtyconsym: $ => choice($.qualified_type_operator, $.type_operator),
  _qatyconsym: $ => parens($._qtyconsym),

  _ticked_qtycon: $ => ticked($._qtycon),
  _tyconops: $ => choice(alias($._ticked_qtycon, $.ticked), $._qtyconsym),
  _promoted_tyconop: $ => seq(quote, $._tyconops),
  tyconop: $ => choice(
    alias($._promoted_tyconop, $.promoted),
    $._tyconops,
  ),

  con_unit: _ => prec('con_unit', parens()),
  con_list: _ => brackets(),
  tycon_arrow: $ => parens($._arrow),
  con_tuple: $ => parens(repeat1($.comma)),

  type_literal: $ => choice(
    $._literal,
    $.con_unit,
    $.con_list,
    $.con_tuple,
  ),

  _promotable_tycon: $ => choice(
    $._qatyconsym,
    $._qtycon,
  ),

  _promoted_tycon: $ => seq(quote, $._promotable_tycon),

  _gtycon: $ => choice(
    alias($._promoted_tycon, $.promoted),
    $._promotable_tycon,
    $.tycon_arrow,
  ),

  _name: $ => choice($._var, $._con),
  _qname: $ => choice($._qvar, $._qcon),
}
