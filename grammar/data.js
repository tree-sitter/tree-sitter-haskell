const {parens} = require('./util.js')

// Moving this to _btype increases gen/compile time by 20%
con_type = ($, t) => choice($.type_strict, $.type_lazy, t)

module.exports = {

  // ------------------------------------------------------------------------
  // gadt
  // ------------------------------------------------------------------------

  _gadt_fun: $ => prec.right('fun', seq($._con_btype, $._fun_arrow, $._gadt_sig)),

  _gadt_linear_fun: $ => prec.right('fun', seq($._con_btype, $._linear_fun_arrow, $._gadt_sig)),

  _gadt_sig: $ => choice(
    alias($._gadt_fun, $.type_fun),
    alias($._gadt_linear_fun, $.type_linear_fun),
    $._btype,
  ),

  _gadt_record: $ => prec.right('fun', seq(
    $.record_fields,
    $._fun_arrow,
    $._gadt_sig,
  )),

  /**
   * gadt constructors only allow single foralls and contexts
   */
  _gadt_constr_type: $ => seq(
    $._colon2,
    prec('fun', optional($.forall)),
    optional($.context),
    choice($._gadt_sig, $._gadt_record),
  ),

  gadt_constructor: $ => seq(
    field('name', choice(
      $._con,
      alias($._con_binding_list, $.binding_list),
    )),
    $._gadt_constr_type,
  ),

  gadt_constructors: $ => layout($, $.gadt_constructor),

  _gadt: $ => seq(
    optional($._type_annotation),
    $._where,
    field('where', optional($.gadt_constructors)),
  ),

  // ------------------------------------------------------------------------
  // adt
  // ------------------------------------------------------------------------

  _con_btype: $ => con_type($, $._btype),

  field: $ => prec('annotated', seq(
    sep1(',', $.variable),
    $._colon2,
    con_type($, $._ktype),
  )),

  data_constructor: $ => seq(
    $.constructor,
    repeat(prec('datacon-param', $._con_btype)),
  ),

  data_constructor_infix: $ => prec('infix', seq(
    field('left_operand', $._con_btype),
    field('operator', $._conop),
    field('right_operand', $._con_btype),
  )),

  _record_field: $ => braces($, $.field),

  record_fields: $ => braces($, sep(',', $.field), optional(',')),

  data_constructor_record: $ => seq(
    $.constructor,
    $.record_fields,
  ),

  /**
   * Special constructors occurring in GHC code
   */
  data_constructor_special: $ => choice(
    $.con_unit,
    $.con_list,
    $.type_tuple,
    $.type_unboxed_tuple,
    $.type_unboxed_sum,
  ),

  /**
   * data constructors only allow single foralls and contexts
   */
  constructors: $ => sep1(
    seq(optional($._phantom_bar), '|'),
    seq(
      optional($.forall),
      optional($.context),
      choice(
        $.data_constructor,
        $.data_constructor_infix,
        $.data_constructor_record,
        $.data_constructor_special,
      ),
    )
  ),

  _data_rhs: $ => choice(
    $._type_annotation,
    seq('=', $.constructors),
    $._gadt,
  ),

  _data: $ => seq(
    optional($.context),
    $._btype,
    optional($._data_rhs),
    repeat($.deriving),
  ),

  decl_data: $ => seq(
    optional('type'),
    'data',
    $._data,
  ),

  // ------------------------------------------------------------------------
  // newtype
  // ------------------------------------------------------------------------

  newtype_constructor: $ => seq(
    $.constructor,
    $._atype,
  ),

  newtype_constructor_record: $ => seq(
    $.constructor,
    $._record_field,
  ),

  _newtype: $ => seq(
    optional($.context),
    $._btype,
    choice(
      seq('=', choice($.newtype_constructor, $.newtype_constructor_record)),
      $._gadt,
    ),
    repeat($.deriving),
  ),

  decl_newtype: $ => seq(
    'newtype',
    $._newtype,
  ),

  // ------------------------------------------------------------------------
  // deriving
  // ------------------------------------------------------------------------

  via: $ => seq('via', $._ktype),

  deriving_strategy: _ => choice('stock', 'newtype', 'anyclass'),

  _deriving_class: $ => $._tyconids,

  deriving: $ => seq(
    optional($._phantom_deriving),
    'deriving',
    optional($.deriving_strategy),
    choice(
      field('class', alias($._deriving_class, $.class_name)),
      parens($, optional(sep1(',', field('class', $._constraint))))
    ),
    optional($.via),
  ),

  // ------------------------------------------------------------------------
  // data family
  // ------------------------------------------------------------------------

  _datafam: $ => seq(
    $._btype,
    optional($._type_annotation),
  ),

  decl_datafam: $ => seq(
    'data',
    'family',
    $._datafam,
  ),

  _inst_adt: $ => seq(
    optional($.context),
    $._btype,
    optional($._data_rhs),
    repeat($.deriving),
  ),

  decl_inst_adt: $ => seq(
    'data',
    'instance',
    optional($.forall),
    $._inst_adt,
  ),

  _inst_newtype: $ => seq(
    optional($.context),
    $._btype,
    choice(
      seq('=', $.newtype_constructor),
      $._gadt,
    ),
    repeat($.deriving),
  ),

  decl_inst_newtype: $ => seq(
    'newtype',
    'instance',
    optional($.forall),
    $._inst_newtype,
  ),

  decl_datainst: $ => choice(
    alias($.decl_inst_adt, $.data_type),
    alias($.decl_inst_newtype, $.newtype),
  ),

}
