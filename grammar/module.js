const {parens} = require('./util.js')

module.exports = {

  // ------------------------------------------------------------------------
  // module names
  // ------------------------------------------------------------------------

  _modid: $ => alias($.constructor, $.module),

  _module_segment: $ => prec('qualifying-module', seq($._modid, $._any_tight_dot)),

  _qualifying_module: $ => repeat1($._module_segment),

  qualified_module: $ => qualified($, $._modid),
  _qmodid: $ => choice($.qualified_module, alias($.constructor, $.module)),

  // ------------------------------------------------------------------------
  // exports
  // ------------------------------------------------------------------------

  export_names: $ => parens($, sep(',', choice(alias('..', $.all_names), $._name))),

  export: $ => choice(
    $._varids,
    seq(
      optional($.namespace),
      choice(
        parens($, $.operator),
        parens($, $._operator_minus),
        parens($, $.qualified_operator),
        parens($, $._consyms),
        $._tyconids,
      ),
      optional($.export_names),
    ),
    seq('module', field('module', $._qmodid)),
  ),

  exports: $ => parens(
    $,
    optional(sep1(',', $.export)),
    optional(','), // for trailing commas at the end of an export list
  ),

  // ------------------------------------------------------------------------
  // module body / sections
  // ------------------------------------------------------------------------

  header: $ => seq(
    'module',
    field('module', $._qmodid),
    field('exports', optional($.exports)),
    $._where,
  ),

  imports: $ => seq(semis($, alias($.decl_import, $.import)), semi($)),

  /**
   * Using `semi` at the end instead of `semi_opt` increases parser size by a full megabyte!!
   */
  declarations: $ => seq(semis($, $._topdecl), semi_opt($)),

  _body: $ => seq(
    choice($._cmd_layout_start, alias($._cmd_layout_start_explicit, '{')),
    semi_opt($),
    field('imports', optional($.imports)),
    field('declarations', optional($.declarations)),
    $._layout_end,
  ),

  _layout_end: $ => choice(
    $._cond_layout_end,
    alias($._cond_layout_end_explicit, '}'),
  ),

}
