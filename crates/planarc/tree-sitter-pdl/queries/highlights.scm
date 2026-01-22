; Keywords
"import" @keyword
"match" @keyword
"where" @keyword
"operator" @storage.type
"query" @storage.type
"type" @storage.type
"extern" @storage.type

; Variables and Constants
(variable) @variable
(identifier) @variable

; Operators & Punctuation
"=" @operator
"->" @operator
"<-" @operator
"<->" @operator
":" @punctuation.delimiter
"{" @punctuation.bracket
"}" @punctuation.bracket
"," @punctuation.delimiter
"?" @storage.type
(operator_identifier) @operator
(refinement (it) @storage.type)


(type_definition type: (type_annotation (type_identifier) @type))

; Types and Properties
(extern_def_arg type: (type_annotation) @type)
(extern_return (type_annotation) @type)
(variable (property_access) @property)
(type_declaration (identifier) @type)
(type_declaration "type" @storage.type)

; Functions and Calls
(call_func (fqmn) @function.call)
(extern_def_fn (identifier) @function)
(extern_def_fn (operator_identifier) @function)




; Specific Contexts for Identifiers
(node_definition kind: (fqmn) @type)
(import_definition (fqmn) @module)
(query_definition
    name: (identifier) @function
    value: (query_literal
        (raw_content) @injection.content
    )
)

(#set! injection.language "query")

(graph_bind
  (graph_left_statements
    (identifier) @variable.builtin
    (#eq? @variable.builtin "global")))

(capture
  (variable) @label) 

; Literals
(string) @string
(query_literal) @string
; (boolean) @boolean
; (number) @number.decimal

; Comments
(comment) @comment