const {parens} = require('./util.js')

module.exports = {
  // ------------------------------------------------------------------------
  // import
  // ------------------------------------------------------------------------

  _import_name: $ => choice(
    $._con,
    $._var,
  ),

  namespace: _ => choice('pattern', 'type'),

  import_con_names: $ => parens(optional(choice($.dotdot, sep1($.comma, $._import_name)))),

  import_item: $ => seq(
    optional($.namespace),
    choice(
      $._var,
      seq($._con, optional($.import_con_names)),
    ),
  ),

  import_list: $ => seq(optional('hiding'), parens(optional(sep1($.comma, $.import_item)))),

  decl_import: $ => seq(
    'import',
    optional(alias($.string, $.import_package)),
    optional('qualified'),
    $._qmodid,
    optional(seq('as', $._qmodid)),
    optional($.import_list),
  ),
}
