const {parens} = require('./util.js')

module.exports = {

  // ------------------------------------------------------------------------
  // fixity
  // ------------------------------------------------------------------------

  // GHC.Types special decl
  _fun_arrow_fixity: $ => seq(
    field('associativity', 'infixr'),
    '-',
    field('precedence', alias('1', $.integer)),
    field('operator', alias('->', $.arrow)),
  ),

  fixity: $ => choice(
    $._fun_arrow_fixity,
    seq(
      field('associativity', choice('infixl', 'infixr', 'infix')),
      field('precedence', optional($.integer)),
      field('operator', sep1(',', choice($._operator_minus, $._varop, $._conop))),
    ),
  ),

  // ------------------------------------------------------------------------
  // signature
  // ------------------------------------------------------------------------

  _con_binding_list: $ => sep2(',', $._con),

  _var_binding_list: $ => sep2(',', $._var),

  signature: $ => seq(
    field('name', choice(
      $._var,
      alias($._var_binding_list, $.binding_list),
    )),
    field('type', $._type_annotation),
  ),

  _gendecl: $ => choice(
    $.signature,
    $.fixity,
  ),

  // ------------------------------------------------------------------------
  // function and pattern bind
  // ------------------------------------------------------------------------

  /**
  * Declare a `varop`.
  */
  _funlhs_infix: $ => prec.right('infix', seq(
    field('left_operand', $._infixpat),
    field('operator', choice($._operator_minus, $._varop)),
    field('right_operand', $._infixpat),
  )),

  guard_equation: $ => seq($._guards, '=', $._cmd_texp_end, field('expression', $._exp)),

  _fun_guards: $ => repeat1($.guard_equation),

  _funrhs: $ => seq(
    choice(
      seq('=', field('expression', $._exp)),
      $._fun_guards,
    ),
    optional($._where_binds),
  ),

  /**
    * The `implicit_parid` here is for:
    * g = let ?par = Impy 5 in f
    */
  _fun_name: $ => choice($._var, $.implicit_parid),

  _funvar: $ => seq(
    field('name', $._fun_name),
    optional($.patterns)
  ),

  _funlhs_parens: $ => choice(
    $._funvar,
    $._funlhs_common,
  ),

  /**
   * Patterns following a parenthesized pattern are always optional for simplicity, even though GHC requires them for
   * the outermost set.
   */
  _funlhs_common: $ => choice(
    alias($._funlhs_infix, $.infix),
    seq(
      parens($, $._funlhs_parens),
      optional($.patterns)
    ),
  ),

  _funlhs: $ => choice(
    seq(
      field('name', $._fun_name),
      $.patterns
    ),
    $._funlhs_common,
  ),

  /**
   * This little contortion ensures that a nullary function declaration/variable binding is not considered for a dynamic
   * conflict:
   *
   * > fun = exp
   *
   * vs.
   *
   * > fun a b c = exp
   * > Con a b c = exp
   *
   * This works by creating a shift/reduce conflict between `function` and `bind`.
   * The shift step is from `_var` to `_funrhs` (the latter of which starts with either `=` or `|`, so lookahead is
   * clear-cut) and the reduce step is from `_var` to `pat_name`.
   * The shift precedence is set by giving _both_ nodes the prec 'function-nullary', and the reduce precedence is set by
   * giving `_var` in `pat_name` the prec 'pat-name'.
   * An entry in the `precedences` config in `grammar.js` defines the order of these precedences, causing the parser to
   * create only a state transition to `_funrhs` when encountering a `=` or `|` after a `variable`.
   */
  function: $ => choice(
    prec('function-nullary', seq(
      field('name', choice($._var, $.implicit_parid)),
      $._funrhs,
    )),
    seq(
      $._funlhs,
      $._funrhs,
    ),
  ),

  bind: $ => seq(
    field('pattern', $._pat),
    $._funrhs,
  ),

  /**
  * The difference between a `function` with a `_funlhs_infix` and a `bind` is that the former is for _declaring_ a
  * `varop` and the latter uses a `conop` to pattern match on the rhs expression.
  * The former may not have a type annotation, while the latter may.
  *
  * > h : t :: [Int] = undefined
  * > a <> b = undefined
  */
  _decl: $ => choice(
    $._gendecl,
    $.function,
    $.bind,
  ),

  binds: $ => layout($, $._decl),

  _binds: $ => seq($._where, optional($.binds)),

  _where_binds: $ => field('where', $._binds),

  // ------------------------------------------------------------------------
  // foreign
  // ------------------------------------------------------------------------

  calling_convention: $ => choice(
    'ccall',
    'stdcall',
    'capi',
    'prim',
    'javascript',
    $._id, // It's common in GHC to use a cpp #define for this
  ),

  safety: _ => token(choice(
    'unsafe',
    'safe',
    'interruptible',
  )),

  entity: $ => $.string,

  decl_foreign_import: $ => seq(
    'foreign',
    'import',
    $.calling_convention,
    optional($.safety),
    optional($.entity),
    $.signature,
  ),

  decl_foreign_export: $ => seq(
    'foreign',
    'export',
    $.calling_convention,
    optional($.entity),
    $.signature,
  ),

  _decl_foreign: $ => choice(
    alias($.decl_foreign_import, $.foreign_import),
    alias($.decl_foreign_export, $.foreign_export),
  ),

  // ------------------------------------------------------------------------
  // default
  // ------------------------------------------------------------------------

  decl_default: $ => seq('default', parens($, optional(sep1(',', $._ktype)))),

}
