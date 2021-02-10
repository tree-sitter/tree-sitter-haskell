const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // type
  // ------------------------------------------------------------------------

  tyvar: $ => $._varid,

  annotated_tyvar: $ => parens($.tyvar, $._type_annotation),

  _tyvar: $ => choice(
    $.annotated_tyvar,
    $.tyvar,
  ),

  _forall_kw: _ => choice('forall', '∀'),

  forall_dot: $ => choice('.', $.arrow),

  _forall: $ => seq(
    $._forall_kw,
    repeat1($._tyvar),
  ),

  forall: $ => seq(
    $._forall,
    $.forall_dot,
  ),

  type_parens: $ => parens($._type),

  list_type: $ => seq(optional(quote), brackets(sep1($.comma, $._type))),

  tuple_type: $ => seq(
    choice(/'\s*\(/, '('),
    $._type,
    $.comma,
    sep1($.comma, $._type),
    ')',
  ),

  strict_type: $ => seq('!', $._atype),

  type_name: $ => choice(
    $._tyvar,
    $._gtycon,
  ),

  _atype: $ => choice(
    $.type_name,
    $.type_literal,
    $.star,
    $.tuple_type,
    $.list_type,
    $.type_parens,
  ),

  /**
   * Type application, as in `Either e (Int, Text)`.
   */
  type_apply: $ => seq($._atype, repeat1($._atype)),

  /**
   * The point of this `choice` is to get a node for type application only if there is more than one atype present.
   */
  _btype: $ => choice(
    $._atype,
    $.type_apply,
  ),

  type_infix: $ => prec('infix-type', seq(
    field('left', $._btype),
    field('op', $.tyconop),
    field('right', $._type_infix),
  )),

  _type_infix: $ => choice(
    $.type_infix,
    prec('btype', $._btype),
  ),

  implicit_param: $ => seq(
    $.implicit_parid,
    $._type_annotation,
  ),

  constraint: $ => choice(
    seq(field('class', alias($.type_name, $.class_name)), repeat($._atype)),
    $.type_infix,
  ),

  _context: $ => seq(
    choice(
      $.constraint,
      prec('context-empty', parens(optional(sep1($.comma, choice($.constraint, $.implicit_param))))),
    ),
  ),

  context: $ => seq($._context, $.carrow),

  _type_quantifiers: $ => seq(
    alias($._forall, $.quantifiers),
    $.forall_dot,
    $._type,
  ),

  _type_context: $ => seq(
    $._context,
    $.carrow,
    $._type,
  ),

  _type_fun: $ => prec('function-type', seq($._type_infix, $.arrow, $._type)),

  _type: $ => prec('type', choice(
    alias($._type_quantifiers, $.forall),
    alias($._type_context, $.context),
    alias($._type_fun, $.fun),
    $._type_infix,
  )),

  _type_annotation: $ => seq(
    $.annotation,
    field('type', $._type),
  ),

  _simpletype_infix: $ => seq(
    $._tyvar,
    field('name', alias($._tyconsym, $.tyconsym)),
    $._tyvar,
  ),

  _simpletype: $ => choice(
    parens($._simpletype),
    alias($._simpletype_infix, $.type_infix),
    seq(
      field('name', $._qtycon),
      repeat($._tyvar),
    ),
  ),

  // ------------------------------------------------------------------------
  // type decl
  // ------------------------------------------------------------------------

  decl_type: $ => seq(
    'type',
    $._simpletype,
    $.equals,
    $._type,
  ),

  decl_type_sig: $ => seq(
    'type',
    optional('family'),
    $._simpletype,
    $._type_annotation,
  ),

  // ------------------------------------------------------------------------
  // type family
  // ------------------------------------------------------------------------

  tyfam_sig: $ => seq(
    // optional(sep1($.comma, choice($._variable, $.implicit_parid))),
    $.annotation,
    $._type
  ),

  tyfam_head: $ => $._simpletype,

  tyfam_pat: $ => seq(
    field('name', $._qtycon),
    repeat($._atype),
  ),

  tyfam_eq: $ => seq(
    $.tyfam_pat,
    $.equals,
    $._type,
  ),

  decl_tyfam: $ => seq(
    'type',
    'family',
    $.tyfam_head,
    optional($.tyfam_sig),
    optional(where($, $.tyfam_eq)),
  ),

  decl_tyinst: $ => seq(
    'type',
    'instance',
    repeat($._atype),
    $.equals,
    $._type,
  ),
}
