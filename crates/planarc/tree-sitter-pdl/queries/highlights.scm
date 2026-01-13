; Keywords
"schema" @keyword
"grammar" @keyword
"import" @keyword
"extern" @keyword
"node" @keyword
"match" @keyword
"query" @keyword
"operator" @keyword

; Operators & Punctuation
"=" @operator
"::" @operator
"->" @operator
"<-" @operator
"<->" @operator
":" @punctuation.delimiter
"{" @punctuation.bracket
"}" @punctuation.bracket
"," @punctuation.delimiter
"?" @operator
(operator_identifier) @operator

; Types and Properties
(extern_def_arg type: (identifier) @type)
(extern_return (identifier) @type)
(variable (property_access) @property)

; Functions and Calls
(call_func (fqmn) @function.call)
(extern_def_fn (identifier) @function)

; Variables and Constants
(variable) @variable
(identifier) @variable

; Specific Contexts for Identifiers
(node_definition kind: (fqmn) @type)
(import_definition (fqmn) @module)
(extern_definition module: (fqmn) @module)

(query_definition name: (identifier) @constant)
(header name: (string) @string.special) ; Schema name is special

(graph_bind
  (graph_left_statements
    (identifier) @variable.builtin
    (#eq? @variable.builtin "global")))

(capture
  (variable) @label) 

; Literals
(string) @string
(raw_string) @string
; (boolean) @boolean
; (number) @number.decimal

; Comments
(comment) @comment