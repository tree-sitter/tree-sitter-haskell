const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // var
  // ------------------------------------------------------------------------

  _group: _ => 'group',

  _variable_group: $ => $._group,

  variable: $ => choice(
    'using',
    $._variable_group,
    'by',
    $._varid,
  ),
  qualified_variable: $ => qualified($, $.variable),
  _varids: $ => choice($.qualified_variable, $.variable),

  _var: $ => choice($.variable, $._varsym_prefix),
  _qvar: $ => choice($.qualified_variable, $._qualified_operator_prefix),
  _vars: $ => choice($._var, $._qvar),

  _variable_ticked: $ => ticked($.variable),

  _varop: $ => choice($.operator, alias($._variable_ticked, $.infix_id)),

  _qvariable_ticked: $ => ticked($.qualified_variable),

  _varids_ticked: $ => alias(
    choice(
      $._variable_ticked,
      $._qvariable_ticked,
    ),
    $.infix_id,
  ),

  _varops: $ => choice(
    $._varsyms,
    alias($._varids_ticked, $.infix_id),
  ),

  // ------------------------------------------------------------------------
  // con
  // ------------------------------------------------------------------------

  constructor: $ => $._conid,
  qualified_constructor: $ => qualified($, $.constructor),
  _conids: $ => choice($.qualified_constructor, $.constructor),

  _con: $ => choice($.constructor, $._consym_prefix),
  _qcon: $ => choice($.qualified_constructor, $._qualified_constructor_operator_prefix),
  // Prec for conflict between projection and qualified name in `A.b`
  _cons: $ => choice(prec('qcon', $._con), $._qcon),

  _constructor_ticked: $ => ticked($.constructor),
  _conop: $ => choice($._constructor_operator_alias, alias($._constructor_ticked, $.infix_id)),

  _qconstructor_ticked: $ => ticked($.qualified_constructor),

  _conids_ticked: $ => alias(
    choice(
      $._constructor_ticked,
      $._qconstructor_ticked,
    ),
    $.infix_id,
  ),

  _conops: $ => choice(
    $._consyms,
    $._conids_ticked,
  ),

  // ------------------------------------------------------------------------
  // tycon
  // ------------------------------------------------------------------------

  _tyconid: $ => alias($.constructor, $.type),
  qualified_type: $ => qualified($, $._tyconid),
  _tyconids: $ => choice($.qualified_type, $._tyconid),

  _tycon: $ => choice($._tyconid, $._varsym_prefix, $._consym_prefix),
  _qtycon: $ => choice($.qualified_type, $._qsym_prefix),
  _tycons: $ => choice($._tycon, $._qtycon),

  _tycon_ticked: $ => ticked($._tyconid),
  _qtycon_ticked: $ => ticked($.qualified_type),

  _tyconids_ticked: $ => alias(
    choice(
      $._tycon_ticked,
      $._qtycon_ticked,
    ),
    $.infix_id,
  ),

  _tyconops: $ => choice(
    $._sym,
    $._qsym,
    $._operator_minus,
    $._tyconids_ticked,
  ),

  tycon_arrow: $ => parens($, $._arrow),

  _promoted_tycons_alias: $ => seq('\'', prec('promoted-tycon', $._tycons)),

  _promoted_tycons: $ => alias($._promoted_tycons_alias, $.promoted),

  _gtycon: $ => choice(
    $._promoted_tycons,
    $._tycons,
    $.tycon_arrow,
  ),

  _promoted_tyconops_alias: $ => seq('\'', $._tyconops),

  _promoted_tyconops: $ => alias($._promoted_tyconops_alias, $.promoted),

  _tyops: $ => choice(
    $._tyconops,
    $._promoted_tyconops,
  ),

  // ------------------------------------------------------------------------
  // op
  // ------------------------------------------------------------------------

  _op_ticked: $ => choice(
    $._varids_ticked,
    $._conids_ticked,
  ),

  _ops: $ => choice(
    $._varops,
    $._conops,
  ),

  _name: $ => choice($._var, $._con),
  _qname: $ => choice($._vars, $._cons),

  // ------------------------------------------------------------------------
  // gcon
  // ------------------------------------------------------------------------

  con_unit: $ => parens($),
  con_list: $ => brackets($),
  con_tuple: $ => parens($, repeat1(',')),

  literal: $ => choice(
    $._literal,
    $.con_unit,
    $.con_list,
    $.con_tuple,
  ),

}
