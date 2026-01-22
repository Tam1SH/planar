/**
 * @file Planardl grammar for tree-sitter
 * @author Tam1SH <ignatternyev54@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: 'planardl',

  word: $ => $.identifier,

  extras: $ => [
    $.comment,
    $._unicode_space,
  ],

  conflicts: $ => [
    [$._type_primary, $.type_application],
    [$.type_application],
    [$.refinement],
  ],

  rules: {

    source_file: $ => seq(
      repeat(choice(
        $.edge_definition,
        $.query_definition,
        $.node_definition, 
        $.extern_definition,
        $.import_definition,
        $.fact_definition,
        $.type_declaration,
        $._newline
      ))
    ),

    pub: _ => seq('pub', optional(field('pkg', '(pkg)'))),

    node_definition: $ => seq(
      optional($.pub),
      'node',
      field('kind', $.fqmn),
      $.block
    ),
    edge_definition: $ => seq(
      optional($.pub),
      'edge',
      field('name', $.identifier),
      '=',
      field('from', $.type_identifier),
      $.relation,
      field('to', $.type_identifier),
    ),

    query_definition: $ => seq(
      optional($.pub),
      'query',
      field('name', $.identifier),
      ':',
      field('grammar', $.fqmn),
      '=',
      field('value', $.query_literal)
    ),

    type_declaration: $ => seq(
      repeat($.attribute),
      optional($.pub),
      'type',
      field('name', $.identifier),
      '=',
      field('body', $.type_definition),
      $._newline
    ),


    fact_definition: $ => seq(
      repeat($.attribute),
      optional($.pub),
      'fact', field('name', $.identifier),
      '{',
      repeat(choice($.fact_field_definition, $._newline)),
      '}'
    ),

    import_definition: $ => seq(
      'import',
      $.fqmn
    ),

    extern_definition: $ => seq(
      field('attributes', repeat($.attribute)),
      optional($.pub),
      'extern',
      field('block', $.extern_block)
    ),  

    query_literal: $ => seq(
      '`',
      field('content', alias(repeat(/[^`]+/), $.raw_content)),
      '`'
    ),
    attribute: $ => seq(
      '#',
      field('name', $.identifier),
      $._newline
    ),




    type_definition: $ => choice(
      field('type', $.type_annotation),
      seq(
        '{',
        repeat(choice($.type_field_definition, $._newline)),
        '}'
      )
    ),


    type_field_definition: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.type_definition),
    ),

    fact_field_definition: $ => seq(
      repeat($.attribute),
      field('name', $.identifier),
      ':',
      field('type', $.type_annotation),
      $._newline
    ),

    type_annotation: $ => choice(
      prec(2, seq($._type_primary, $.refinement)),
      prec(1, $._type_primary)
    ),

    _type_primary: $ => choice(
      $.type_identifier,                 
      $.type_application,                
      seq('(', $.type_annotation, ')')  
    ),
    
    type_application: $ => seq(
      field('constructor', $.type_identifier),
      repeat1(field('argument', $._type_atom))
    ),

    _type_atom: $ => choice(
      $.type_identifier,
      seq('(', $.type_annotation, ')')
    ),

    refinement: $ => seq('where', repeat($._expression)),

    type_arguments: $ => seq(
      '<',
      commaSep1($.type_annotation),
      '>',
    ),

    _expression: $ => choice(
      $.it,
      $.fqmn,
      $.number,
      $.string,
      $.operator_identifier,
      $.in_expression,
      $.parenthesized_expression
    ),

    parenthesized_expression: $ => seq(
      '(',
      repeat1($._expression),
      ')'
    ),

    it: _ => field('it', 'it'),
    

    list_items: $ => commaSep1($._expression),
    
    in_expression: $ => seq(
      'in',
      '[',
      choice(
        $.range,      
        $.list_items  
      ),
      ']'
    ),

    operator_identifier: $ => /[!@#$%^&*\-+=|<>/?~]+/,

    range: $ => seq(
      field('start', $._expression),
      '..',
      optional(field('end', $._expression)) 
    ),

    type_identifier: $ => $.fqmn,


    
    binary_expression: $ => prec.left(1, seq(
        field('left', $._expression),
        field('operator', choice('+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=')),
        field('right', $._expression)
    )),
    



    extern_def_fn: $ => seq(
      choice(
        $.identifier,
        seq(
          'operator',
          $.operator_identifier,
        )
      ),

      repeat(seq($.extern_def_arg, optional(','))),
      '->',
      $.extern_return,
      $._newline
    ),

    extern_return: $ => seq(
      $.type_annotation, optional('?')
    ),

    extern_def_arg: $ => seq(
      field('arg', $.identifier), ':', field('type', $.type_annotation)
    ),

    extern_block: $ => seq(
      '{',
      repeat(choice($.extern_def_fn, $._newline)),
      '}'
    ),
    


    
    block: $ => seq(
      '{',
      repeat(choice($._statement, $._newline)),
      '}'
    ),

    _statement: $ => choice(
      $.match_stmt,
      $.query_definition
    ),

    match_stmt: $ => seq(
      'match',
      field('query', choice($.query_literal, $.identifier)),
      $.match_block
    ),

    match_block: $ => seq(
      '{',
      repeat(choice($._match_statements, $._newline)),
      '}'
    ),

    _match_statements: $ => choice(
      $.capture
    ),

    capture: $ => seq(
      $.variable,
      $.capture_block
    ),

    _capture_statements: $ => choice(
      $.graph_bind
    ),

    graph_left_statements: $ => choice(
      $.variable,
      $.identifier
    ),

    graph_right_statements: $ => choice(
      $.call_func
    ),
    
    call_func: $ => seq(
      field('function', $.fqmn),
      repeat(field('arg', $.variable)),
      $._newline,
    ),

    relation: _ => choice('<-', '->', '<->'),

    graph_bind: $ => seq(
      field('left', $.graph_left_statements),
      field('relation', $.relation),
      field('right', $.graph_right_statements)
    ),

    capture_block: $ => seq(
      '{',
      repeat(choice($._capture_statements, $._newline)),
      '}'
    ),

    fqmn: $ => seq(
      $.identifier,      
      repeat(seq(
        '.',                         
        $.identifier     
      ))
    ),

    _simple_variable: $ => choice(
      /@[a-zA-Z_][a-zA-Z0-9_]*/,
      /\$[a-zA-Z_][a-zA-Z0-9_]*/
    ),

    property_access: $ => seq(
      '.',
      /[a-zA-Z_][a-zA-Z0-9_]*/
    ),

    variable: $ => seq(
      $._simple_variable,
      repeat($.property_access)
    ),


    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_-]*/,

    boolean: $ => choice('true', 'false'),
    
    number: $ => /\d+/,

    string: $ => token(seq(
      '"',
      repeat(choice(
        /[^"\\\n]+/,
        /\\./
      )),
      '"'
    )),

    
    raw_string: $ => seq(
      '`',
      /[^`]+/,
      '`'
    ),

    // unicode-space := See Table (All White_Space unicode characters which are not `newline`)
    // Whitespace
    // The following characters should be treated as non-Newline white space:
    //
    // ╭────────────────────────────────────╮
    // │  Name                      Code Pt │
    // │  Character Tabulation      U+0009  │
    // │  Space                     U+0020  │
    // │  No-Break Space            U+00A0  │
    // │  Ogham Space Mark          U+1680  │
    // │  En Quad                   U+2000  │
    // │  Em Quad                   U+2001  │
    // │  En Space                  U+2002  │
    // │  Em Space                  U+2003  │
    // │  Three-Per-Em Space        U+2004  │
    // │  Four-Per-Em Space         U+2005  │
    // │  Six-Per-Em Space          U+2006  │
    // │  Figure Space              U+2007  │
    // │  Punctuation Space         U+2008  │
    // │  Thin Space                U+2009  │
    // │  Hair Space                U+200A  │
    // │  Narrow No-Break Space     U+202F  │
    // │  Medium Mathematical Space U+205F  │
    // │  Ideographic Space         U+3000  │
    // ╰────────────────────────────────────╯
    _unicode_space: _ =>
      /[\u0009\u0020\u00A0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]/,

    // newline := See Table (All line-break white_space)
    // Newline
    // The following characters should be treated as new lines:
    //
    // ╭──────────────────────────────────────────────────────────╮
    // │  Acronym  Name                           Code Pt         │
    // │  CR       Carriage Return                U+000D          │
    // │  LF       Line Feed                      U+000A          │
    // │  CRLF     Carriage Return and Line Feed  U+000D + U+000A │
    // │  NEL      Next Line                      U+0085          │
    // │  FF       Form Feed                      U+000C          │
    // │  LS       Line Separator                 U+2028          │
    // │  PS       Paragraph Separator            U+2029          │
    // ╰──────────────────────────────────────────────────────────╯
    // Note that for the purpose of new lines, CRLF is considered a single newline.
    _newline: _ => choice(/\r/, /\n/, /\r\n/, /\u0085/, /\u000C/, /\u2028/, /\u2029/),

    comment: $ => token(seq('//', /.*/)),
  }
});

/**
 * @param {RuleOrLiteral} rule
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

/**
 * @param {RuleOrLiteral} rule
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

/**
 * @param {RuleOrLiteral} rule
 * @param {RuleOrLiteral} separator
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
