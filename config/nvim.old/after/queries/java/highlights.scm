;; extends

((modifiers ("public") @keyword) (#set! conceal " "))
((modifiers ("private") @keyword) (#set! conceal " "))
((modifiers ("static") @keyword) (#set! conceal " "))
((_ (void_type) @type) (#set! conceal " "))
((class_declaration ("class") @keyword) (#set! conceal " "))
