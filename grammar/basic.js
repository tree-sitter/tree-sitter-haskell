const
  digit = /[0-9]/
  digit_hex = /[0-9a-fA-F]/
  digit_octal = /[0-7]/
  decimals = repeat1(digit)

  // TODO why not regex? is it legal to have whitespace inbetween?
  hex = seq(choice('x', 'X'), repeat1(digit_hex))
  octal = seq(choice('o', 'O'), repeat1(digit_octal))

  literal_decimal = seq(digit, repeat(digit))
  literal_hex = seq('0', hex)
  literal_octal = seq('0', octal)

  exponent = seq(
    choice('e', 'E'),
    optional(choice('+', '-')),
    repeat1(literal_decimal)
  )

  literal_float = choice(
    seq(decimals, '.', decimals, optional(exponent)),
    seq(decimals, exponent)
  )

module.exports = {
  // ------------------------------------------------------------------------
  // literals
  // ------------------------------------------------------------------------

  float: _ => token(literal_float),

  char: _ => /'([A-Za-z0-9();\[\]`ʹ{}_!#$%&⋆+,./<=>?@^" |\-~:\\*]|\\[a-zA-Z0-9\\"'&]*|\\\^[0-9A-Z@\[\]\\\^_])'/,

  string: _ => token(seq(
    '"',
    repeat(choice(
      /[^\\"\n]/,
      /\\(\^)?./,
      /\\\n\s*\\/,
    )),
    '"',
  )),

  _integer_literal: _ => token(literal_decimal),
  _octal_literal: _ => token(literal_octal),
  _hex_literal: _ => token(literal_hex),

  integer: $ => choice(
    $._integer_literal,
    $._octal_literal,
    $._hex_literal,
  ),

  _literal: $ => choice(
    $.integer,
    $.float,
    $.string,
    $.char,
  ),

  // ------------------------------------------------------------------------
  // pragma
  // ------------------------------------------------------------------------

  pragma: _ => token(
    seq(
      '{-#',
      repeat(choice(
        /[^#]/,
        /#[^-]/,
        /#\-[^}]/,
      )),
      '#-}'
    )
  ),

  // ------------------------------------------------------------------------
  // reserved symbols
  // ------------------------------------------------------------------------

  _semicolon: _ => ';',
  wildcard: _ => '_',
  dotdot: _ => '..',
  arrow: _ => '->',
  larrow: _ => '<-',
  carrow: _ => '=>',
  lambda: _ => '\\',
  tyapp: _ => '@',
  strict: _ => '!',
  bar: _ => '|',
  equals: _ => '=',
  annotation: _ => '::',
  as_pat: _ => token.immediate('@'),
  star: _ => '*',
}
