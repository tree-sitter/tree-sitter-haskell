module.exports = {

  // ------------------------------------------------------------------------
  // associated families
  // ------------------------------------------------------------------------

  /**
   * In associated family declarations, result types aliasing without injectivity is invalid, since that syntax is taken
   * by type instance declarations.
   */
  _assoc_tyfam: $ => seq(
    'type',
    optional('family'),
    alias($._btype, $.head),
    optional(choice(
      $._type_annotation,
      seq(
        $.type_family_result,
        $.type_family_injectivity,
      ),
    )),
  ),

  _assoc_tyinst: $ => seq(
    'type',
    optional('instance'),
    $._type_instance,
  ),

  _assoc_datafam: $ => seq(
    'data',
    optional('family'),
    $._datafam,
  ),

  _assoc_datainst_adt: $ => seq(
    'data',
    optional('instance'),
    optional($.forall),
    $._inst_adt,
  ),

  _assoc_datainst_newtype: $ => seq(
    'newtype',
    optional('instance'),
    optional($.forall),
    $._inst_newtype,
  ),

  _assoc_datainst: $ => choice(
    alias($._assoc_datainst_adt, $.data_type),
    alias($._assoc_datainst_newtype, $.newtype),
  ),

  // ------------------------------------------------------------------------
  // class
  // ------------------------------------------------------------------------

  default_signature: $ => seq('default', $.signature),

  _cdecl: $ => choice(
    $._decl,
    $.default_signature,
    alias($._assoc_tyfam, $.type_family),
    alias($._assoc_tyinst, $.type_instance),
    alias($._assoc_datafam, $.data_family),
  ),

  fundep: $ => seq(repeat1($.type_variable), $._arrow, repeat1($.type_variable)),

  fundeps: $ => seq(optional($._phantom_bar), '|', sep1(',', $.fundep)),

  class_declarations: $ => layout($, $._cdecl),

  _where_class_body: $ => seq($._where, optional($.class_declarations)),

  decl_class: $ => seq(
    'class',
    optional($.context),
    field('head', alias($._kclass, $.class_head)),
    optional($.fundeps),
    optional(field('where', optional($._where_class_body))),
  ),

  // ------------------------------------------------------------------------
  // instance
  // ------------------------------------------------------------------------

  _idecl: $ => choice(
    $._decl,
    alias($._assoc_datainst, $.data_instance),
    alias($._assoc_tyinst, $.type_instance),
  ),

  instance_declarations: $ => layout($, $._idecl),

  /**
   * instances only allow single foralls and contexts
   */
  _instance: $ => seq(
    'instance',
    field('forall', optional($.forall)),
    field('context', optional($.context)),
    field('head', alias($._kclass, $.instance_head)),
  ),

  decl_instance: $ => seq(
    $._instance,
    optional(seq($._where, field('where', optional($.instance_declarations)))),
  ),

  decl_deriving: $ => seq(
    optional($._phantom_deriving),
    'deriving',
    optional(choice($.deriving_strategy, $.via)),
    $._instance,
  ),
}
