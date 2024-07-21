;; extends

;; types
; (((_ (primitive_type) @type) (#eq? @type "int")) (#set! conceal "ℤ"))

;; qualifiers
; ((sized_type_specifier ("unsigned") @type) (#offset! 0 0 0 2) (#set! conceal "−̷"))
; (((_ (type_qualifier) @keyword) (#eq? @keyword "const")) (#set! conceal "C"))
; (((_ (storage_class_specifier) @keyword) (#eq? @keyword "static")) (#set! conceal "S"))
; ((struct_specifier ("struct") @keyword.type) (#set! conceal " "))

;; primitive concepts
; ((_ (null) @keyword) (#set! conceal "∅"))
; ((return_statement ("return") @keyword) (#set! conceal "↵"))
; ((for_expression ("for") @keyword) (#set! conceal "∀"))
