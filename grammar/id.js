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
  conid: $ => $._conid,
  qconid: $ => qualified($, $.conid),
  _qconid: $ => choice($.qconid, $.conid),

  consym: $ => $._consym,
  qconsym: $ => qualified($, $.consym),
  _qconsym: $ => choice($.qconsym, $.consym),

  _con: $ => choice($.conid, parens($.consym)),
  _qcon: $ => choice($._qconid, parens($._qconsym)),
  _conop: $ => choice($.consym, ticked($.conid)),
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
  // tyconid
  // ------------------------------------------------------------------------

  tyconid: $ => $.conid,
  qtycon: $ => qualified($, $.tyconid),
  _qtycon: $ => choice($.qtycon, $.tyconid),

  tyconsym: $ => $._tyconsym,
  qtyconsym: $ => qualified($, $.tyconsym),
  _qtyconsym: $ => choice($.qtyconsym, $.tyconsym),
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
  tycon_arrow: $ => parens($.arrow),
  con_tuple: $ => parens(repeat1($.comma)),

  _type_literal: $ => choice(
    $.con_unit,
    $.con_list,
    $.con_tuple,
  ),

  _promoted_type_literal: $ => choice(
    seq(quote, $._type_literal)
  ),

  type_literal: $ => choice(
    $._literal,
    $._type_literal,
    alias($._promoted_type_literal, $.promoted),
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
