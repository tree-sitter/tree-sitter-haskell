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

  import_con_names: $ => parens($, optional(choice(alias('..', $.all_names), sep1(',', $._import_name)))),

  import_item: $ => choice(
    $.variable,
    seq(
      optional($.namespace),
      choice(
        $._tyconid,
        $._varsym_prefix,
        $._consym_prefix,
      ),
      optional($.import_con_names),
    ),
  ),

  import_list: $ => seq(
    optional('hiding'),
    parens($, optional(seq(
      sep1(',', $.import_item),
      optional(','), // for trailing commas at the end of an import list
    ))),
  ),

  decl_import: $ => seq(
    'import',
    optional('qualified'),
    optional(alias($.string, $.import_package)),
    $._qmodid,
    optional('qualified'),
    optional(seq('as', $._qmodid)),
    optional($.import_list),
  ),
}
