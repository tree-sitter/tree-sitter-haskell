const

parens = ($, ...rule) => seq($._paren_open, ...rule, $._paren_close)

braces = ($, ...rule) => seq('{', $._cmd_brace_open, ...rule, '}', $._cmd_brace_close)

brackets = ($, ...rule) => seq($._bracket_open, ...rule, $._bracket_close)

ticked = (...rule) => seq('`', ...rule, '`')

qualified = ($, id) => prec('qualified-id', seq($._qualifying_module, id))

sep = (sep, rule) => optional(seq(rule, repeat(seq(sep, rule))))

sep1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)))

sep2 = (sep, rule) => seq(rule, repeat1(seq(sep, rule)))

// ------------------------------------------------------------------------
// semicolon
// ------------------------------------------------------------------------

semi = $ => choice(repeat1(';'), $._cond_layout_semicolon)

semi_opt = $ => optional(semi($))

semis = ($, rule) => sep1(semi($), rule),

// ------------------------------------------------------------------------
// layout
// ------------------------------------------------------------------------

/**
 * Wrap a repeated rule in a layout.
 * This is used for `where`, `let`, `of`, `if` and `do`, and the toplevel module.
 * The `start` rule must be one of the externals starting with `_cmd_layout_<type>`, which instruct the scanner to push
 * a layout context with the current column as its indentation.
 * When a `_cond_layout_end` or `_cond_layout_semicolon` is encountered by the scanner, the recorded indent is compared
 * to the current one to make a decision.
 */
layout_sort = ($, start, rule) => seq(
  choice(start, alias($._cmd_layout_start_explicit, '{')),
  optional(seq(
    semi_opt($),
    semis($, rule),
    semi_opt($),
  )),
  $._layout_end,
),

/**
 * Alias for `layout_sort` using the common layout type for the start token, which corresponds to declarations and GADT
 * constructors.
 */
layout = ($, rule) => layout_sort($, $._cmd_layout_start, rule)

// ------------------------------------------------------------------------
// unboxed
// ------------------------------------------------------------------------

unboxed = ($, ...rules) => seq($._unboxed_open, ...rules, $._unboxed_close)

unboxed_tuple = ($, rule) => unboxed($, sep(',', optional(rule))),

unboxed_sum = ($, rule) => unboxed($, sep2(choice('|', token.immediate('|')), optional(rule))),

// ------------------------------------------------------------------------
// where
// ------------------------------------------------------------------------

optional_where = ($, rule) => optional(seq(
  $._where,
  field('where', optional(rule)),
))

optional_where_as = ($, rule, name) => optional_where($, alias(rule, name))

module.exports = {
  parens,
  braces,
  brackets,
  ticked,
  qualified,
  sep,
  sep1,
  sep2,
  semis,
  layout,
  unboxed,
  optional_where,
  optional_where_as,
}
