const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // adt
  // ------------------------------------------------------------------------

  field_id: $ => $.varid,

  field: $ => seq(
    sep1($.comma, $.field_id),
    $.annotation,
    choice($.strict_type, $._type),
  ),

  constr_id: $ => $._con,

  constr: $ => seq(
    $.constr_id,
    repeat(choice($.strict_type, $._atype))
  ),

  constr_infix: $ => seq(
    choice($.strict_type, $._type_infix),
    $._conop,
    choice($.strict_type, $._type_infix),
  ),

  _record_field: $ => braces($.field),

  record_fields: $ => braces(sep1($.comma, $.field)),

  constr_record: $ => seq(
    $.constr_id,
    $.record_fields,
  ),

  constrs: $ => sep1(
    $.bar,
    seq(
      optional($.forall),
      optional($.context),
      choice(
        $.constr,
        $.constr_infix,
        $.constr_record,
      ),
    )
  ),

  via: $ => seq('via', $._atype),

  _deriving_strategy: _ => choice('stock', 'newtype', 'anyclass'),

  deriving: $ => seq(
    'deriving',
    optional($._deriving_strategy),
    choice(
      field('class', $._qtycon),
      parens(optional(sep1($.comma, field('class', $._qtycon))))
    ),
    optional($.via),
  ),

  _adt_rhs: $ => seq(
    $.equals,
    $.constrs,
    repeat($.deriving),
  ),

  _gadt_fun: $ => seq(choice($.strict_type, $._type_infix), $._arrow, $._gadt_sig),

  _gadt_sig: $ => choice(
    alias($._gadt_fun, $.fun),
    choice($.strict_type, $._type_infix)
  ),

  _gadt_constr_type: $ => seq(
    $.annotation,
    optional($.forall),
    optional($.context),
    choice($._gadt_sig, seq($.record_fields, $._arrow, $._gadt_sig)),
  ),

  gadt_constr: $ => seq(
    $._con,
    $._gadt_constr_type,
  ),

  _gadt_rhs: $ => where($, choice($.gadt_constr, $.deriving)),

  _adt: $ => seq(
    choice($._adt_rhs, $._gadt_rhs),
  ),

  decl_adt: $ => seq(
    'data',
    optional($.context),
    $._simpletype,
    optional($._type_annotation),
    optional($._adt),
  ),

  constr_newtype: $ => seq(
    $.constr_id,
    choice(
      $._atype,
      $._record_field,
    ),
  ),

  _newtype: $ => seq(
    $.equals,
    $.constr_newtype,
    repeat($.deriving),
  ),

  _context_newtype: $ => choice(
    seq($.context, $._simpletype),
    $._simpletype,
  ),

  decl_newtype: $ => seq(
    'newtype',
    $._context_newtype,
    $._newtype
  ),

  // ------------------------------------------------------------------------
  // data family
  // ------------------------------------------------------------------------

  decl_datafam: $ => seq(
    'data',
    'family',
    $._simpletype,
    optional($._type_annotation),
  ),

  _datainst: $ => seq(
    'instance',
    optional($.forall),
    optional($.context),
    $._type_infix,
    optional($._type_annotation),
  ),

  decl_datainst: $ => choice(
    seq(
      'data',
      $._datainst,
      optional($._adt),
    ),
    seq(
      'newtype',
      $._datainst,
      $._newtype
    ),
  ),
}
