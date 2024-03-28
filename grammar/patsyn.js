module.exports = {
  // ------------------------------------------------------------------------
  // pattern synonym
  // ------------------------------------------------------------------------

  /**
   * Equations use `_pat` on the LHS, which includes `pat_annotated`, which clashes with the signature.
   * This uses `_con` for the patsyn name, which is also in `_pat`, so `pattern A :: A` has a shift/reduce conflict
   * before the `::`.
   * We could just use `_pat` here and parse leniently to disambiguate this (because the signature only ever has one
   * token), but that would leave us with a `(pat_name (constructor))` for the signature, which is undesirable.
   *
   * In `_cons`, which is used by `pat_name`, we therefore add a prec ('qcon') for reduction, and a higher prec
   * ('patsyn') here for the shift.
   */
  _patsyn_signature: $ => seq(
    choice(
      prec('patsyn', seq(field('synonym', $._con), $._colon2)),
      seq(field('synonym', alias($._con_binding_list, $.binding_list)), $._colon2),
    ),
    field('type', $._ktype),
  ),

  _patsyn_cons: $ => layout($, alias($.bind, $.constructor_synonym)),

  /**
   * This allows a `where` after any equation for parsing resilience, even though it's only valid for the arrow variant
   * (explicitly bidirectional patterns).
   * The `where` may also be empty.
   */
  _patsyn_equation: $ =>
    prec('patsyn', seq(
      field('synonym', $._pat),
      choice(
        '=',
        $._larrow,
      ),
      field('pattern', $._pat),
      optional_where_as($, $._patsyn_cons, $.constructors),
    )),

  decl_patsyn: $ => seq(
    'pattern',
    choice(
      alias($._patsyn_signature, $.signature),
      alias($._patsyn_equation, $.equation),
    ),
  ),

}
