;; extends

((lambda ("\\") @operator) (#set! conceal "λ"))
(((_ (name) @type) (#eq? @type "Integer")) (#set! conceal "ℤ"))
((_ (unit) @type) (#set! conceal "∅"))
