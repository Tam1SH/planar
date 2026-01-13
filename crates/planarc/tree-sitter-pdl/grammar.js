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

  conflicts: $ => [],

  rules: {

    source_file: $ => seq(
      repeat(choice(
        field('header', $.header),
        $.query_definition,
        $.node_definition, 
        $.extern_definition,
        $.import_definition,
        $.fact_defintion,
        $._newline
      ))
    ),


    
    query_definition: $ => seq(
      'query',
      field('name', $.identifier),
      '=',
      field('value', $.query_literal)
    ),

    query_literal: $ => seq(
      '`',
      field('content', alias(repeat(/[^`]+/), $.raw_content)),
      '`'
    ),

    // schema "docker-compose" grammar="yaml"
    header: $ => seq(
      'schema',
      field('name', $.string),
      'grammar',
      '=',
      field('grammar_ref', $.string)
    ),
    

    fact_field_definition: $ => seq(
      field('name', $.identifier), field('type', $.identifier)
    ),

    fact_defintion: $ => seq(
      'fact',
      $.identifier,
      '{',
      repeat(choice($.fact_field_definition, $._newline)),
      '}'
    ),

    import_definition: $ => seq(
      'import',
      $.fqmn
    ),

    extern_definition: $ => seq(
      'extern',
      field('module', $.fqmn),
      $.extern_block
    ),  

    extern_def_fn: $ => seq(
      choice(
        $.identifier,
        seq(
          'operator',
          $.operator_identifier,
        )
      ),

      '::',
      repeat(seq($.extern_def_arg, optional(','))),
      '->',
      $.extern_return,
      $._newline
    ),
    
    operator_identifier: $ => /[!@#$%^&*\-+=|<>/?~]+/,

    extern_return: $ => seq(
      $.identifier, optional('?')
    ),

    extern_def_arg: $ => seq(
      field('arg', $.identifier), ':', field('type', $.identifier)
    ),

    extern_block: $ => seq(
      '{',
      repeat(choice($.extern_def_fn, $._newline)),
      '}'
    ),
    // node Service { ... }
    node_definition: $ => seq(
      'node',
      field('kind', $.fqmn),
      $.block
    ),

    // { ... statements ... }
    block: $ => seq(
      '{',
      repeat(choice($._statement, $._newline)),
      '}'
    ),

    _statement: $ => choice(
      $.match_stmt
    ),

    
    // match #" ... "#
    match_stmt: $ => seq(
      'match',
      field('query', choice($.raw_string, $.identifier)),
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
      $.fqmn,
      repeat($.variable),
      $._newline,
    ),

    graph_bind: $ => seq(
      $.graph_left_statements,
      field('relation', choice('<-', '->', '<->')),
      $.graph_right_statements
    ),

    capture_block: $ => seq(
      '{',
      repeat(choice($._capture_statements, $._newline)),
      '}'
    ),

    fqmn: $ => seq(
      /[a-zA-Z_][a-zA-Z0-9_]*/,      
      repeat(seq(
        '.',                         
        /[a-zA-Z_][a-zA-Z0-9_]*/     
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

    // KDL-style Raw String: r#"..."# 
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
