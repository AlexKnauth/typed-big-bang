#lang typed/racket

(require "../big-bang.rkt"
         ;"../try-to-use-2htdp/big-bang.rkt"
         typed/2htdp/image)

(define-type World Integer)

(: main : [World -> World])
(define (main t)
  ((inst big-bang World)
   t
   exact-integer?
   #:on-tick add1
   #:to-draw render
   #:stop-when (λ ([t : Integer]) (<= 10000 t))
   #:on-key (λ ([t : Integer] [key : KeyEvent])
              (cond [(key=? key "+") (+ 1000 t)]
                    [else t]))
   #:on-release (λ ([t : Integer] [key : KeyEvent])
                  (- t 100))
   #:on-mouse (λ ([t : Integer] [x : Integer] [y : Integer] [me : MouseEvent])
                (- t 2))
   ))

(: render : [World -> Image])
(define (render t)
  (overlay (text (number->string t) 36 "black")
           (empty-scene 200 200)))

(main 0)
