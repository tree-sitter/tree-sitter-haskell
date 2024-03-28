const {parens} = require('./util.js')

module.exports = {

  // ------------------------------------------------------------------------
  // context
  // ------------------------------------------------------------------------

  class_infix: $ => prec.right('infix', seq(
    field('left_operand', $._btype),
    // This would be nice, but it triggers bug #2580
    // field('class', alias($._tyops, $.class_operator)),
    field('class', $._tyops),
    field('right_operand', $._btype),
  )),

  class_apply: $ =>
    prec.right('apply', seq(
      field('class', alias($._btype, $.class_name)),
      repeat1(field('argument', $._type_apply_arg)),
    )),

  class_parens: $ => parens($, $._kclass),

  context_parens: $ => parens($, $._kconstraint),

  class_annotated: $ => prec.right('annotated', seq(
    $._kclass,
    $._colon2,
    $._ktype,
  )),

  context_annotated: $ => prec.right('annotated', seq(
    $._kconstraint,
    $._colon2,
    $._ktype,
  )),

  constraints: $ => parens($, sep2(',', $._constraint)),

  quantified_constraint: $ => prec.right('fun', seq($._quantifiers, $._constraint)),

  constraint_context: $ => prec.right('fun', seq(
    $._context,
    $._carrow,
    $._constraint,
  )),

  implicit_param: $ => prec.left('implicit', seq(
    $.implicit_parid,
    $._type_annotation,
  )),

  _constraint_implicit: $ => $.implicit_param,

  _kclass: $ => choice(
    $._aclass,
    $.class_infix,
    $.class_apply,
    $.class_annotated,
    $.class_parens,
  ),

  _kconstraint: $ => choice(
    $.context_annotated,
    $.constraints,
    $.context_parens,
    $.quantified_constraint,
    $.constraint_context,
    $._constraint_implicit,
  ),

  _constraint: $ => choice($._kclass, $._kconstraint),

  _context: $ => choice(
    $._kclass,
    $.constraints,
    $.context_parens,
  ),

  context: $ => seq($._context, $._carrow),

  _aclass: $ => choice(
    alias($.type_name, $.class_name),
    $.type_star,
    $._type_literal,
    $.splice,
    $.quasiquote,
    $.type_wildcard,
  ),

}
