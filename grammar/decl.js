const foreign = ($, kw, pent) => seq(
    'foreign',
    kw,
    $._foreign_pre,
    optional(pent),
    $.decl_sig,
  )

module.exports = {
  // ------------------------------------------------------------------------
  // decl
  // ------------------------------------------------------------------------

  _funpat_infix: $ => seq(field('lhs', $._pat), field('op', $.varop), field('rhs', $._pat)),

  funpat: $ => choice(
    alias($._funpat_infix, $.infix),
    $._pat,
  ),

  funvar: $ => seq(field('name', $._var), repeat($._apat)),

  gdrhs: $ => seq($.guards, $.equals, $._exp),

  funrhs: $ => seq(
    choice(
      seq($.equals, $._exp),
      repeat1($.gdrhs),
    ),
    optional(seq($.where, optional($.decls))),
  ),

  decl_fixity: $ => seq(
    choice('infixl', 'infixr', 'infix'),
    optional($.integer),
    sep1($.comma, $._op),
  ),

  /**
    * The `implicit_parid` here is for:
    * f :: (?par :: Impy) => Int
    *
    * g :: Int
    * g = let ?par = Impy 5 in f
    */
  decl_sig: $ => seq(
    sep1($.comma, field('name', choice($._var, $.implicit_parid))),
    $._type_annotation,
  ),

  _gendecl: $ => choice(
    $.decl_sig,
    $.decl_fixity,
  ),

  /**
    * in the reference, `apat` is a choice in `lpat`, but this creates a conflict:
    * `decl` allows the lhs to be a `pat`, as in:
    * let Just 5 = prog
    * let a = prog
    * Since patterns can be `varid`s, the `funpat` lhs of the second example cannot be distinguished from a `funvar`.
    * These precedences solve this.
    */
  decl_fun: $ => seq(
    choice(
      prec.dynamic(2, $.funvar),
      prec.dynamic(1, $.funpat)
    ),
    $.funrhs,
  ),

  _decl: $ => choice(
    $._gendecl,
    $.decl_fun,
  ),

  decls: $ => layouted($, $._decl),

  // ------------------------------------------------------------------------
  // foreign
  // ------------------------------------------------------------------------

  calling_convention: _ => choice(
    'ccall',
    'stdcall',
    'cplusplus',
    'jvm',
    'dotnet',
  ),

  safety: _ => choice('unsafe', 'safe'),

  impent: $ => $.string,

  expent: $ => $.string,

  _foreign_pre: $ => seq(
    $.calling_convention,
    optional($.safety),
  ),

  decl_foreign_import: $ => foreign($, 'import', $.impent),

  decl_foreign_export: $ => foreign($, 'export', $.expent),

  _decl_foreign: $ => choice($.decl_foreign_import, $.decl_foreign_export),
}
