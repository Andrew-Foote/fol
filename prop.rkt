#lang typed/racket
(require syntax/parse/define)

(define-type (Prop a) (U (PropVar a) (Cond a)))
(struct (a) PropVar ([id : a]) #:transparent)
(struct (a) Cond ([ante : (Prop a)] [succ : (Prop a)]) #:transparent)

(: ⇒ (All (a) (-> (Prop a) (Prop a) * (Prop a))))
(define (⇒ φ . ψs)
    (match ψs
      ['()
       φ]
      [(list ψ ξs ...)
       (Cond φ (apply ⇒ ψ ξs))]))

(define-syntax-parser prop
  #:datum-literals (⇒)
  [(_ (⇒ ?φ))
   #'(prop ?φ)]
  [(_ (⇒ ?φ ?ψs ...))
   #'(Cond (prop ?φ) (prop (⇒ ?ψs ...)))]
  [(_ ?A)
   #'(PropVar ?A)])

