;; extends

;; types
(((_ (primitive_type) @type) (#eq? @type "int")) (#set! conceal "ℤ"))
(((_ (primitive_type) @type) (#eq? @type "void")) (#set! conceal "🛇"))
(((_ (primitive_type) @type) (#eq? @type "bool")) (#set! conceal "𝔹"))

;; qualifiers
((sized_type_specifier ("unsigned") @type) (#offset! 0 0 0 2) (#set! conceal "∓̸"))
(((_ (type_qualifier) @keyword) (#eq? @keyword "const")) (#set! conceal "C"))
(((_ (storage_class_specifier) @keyword) (#eq? @keyword "static")) (#set! conceal "S"))
((struct_specifier ("struct") @type) (#set! conceal " "))

;; primitives
((unary_expression ("!") @operator) (#set! conceal "¬"))
((_ (null) @keyword) (#set! conceal "∅"))
((return_statement ("return") @keyword) (#set! conceal "↵"))
; ((for_statement ("for") @keyword) (#set! conceal "∀"))
((binary_expression ("&&") @operator) (#set! conceal "∧"))
((binary_expression ("||") @operator) (#set! conceal "∨"))
