;; extends

((lambda ("\\") @operator) (#set! conceal "λ"))
(((_ (name) @type) (#eq? @type "Integer")) (#set! conceal "ℤ"))
(((_ (name) @type) (#eq? @type "Rational")) (#set! conceal "ℚ"))
((_ (unit) @type) (#set! conceal "∅"))
