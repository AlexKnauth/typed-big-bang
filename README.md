# typed-big-bang

```(require typed/big-bang)```
This does not provide a drop in replacement for big-bang from 2htdp/universe.
Instead of a macro, it provides a function, which has keyword arguments for #:on-tick, #:to-draw, etc.
You also have to provide a `world?` predicate, and use (inst big-bang World).

An example world program:
```
#lang typed/racket

(require typed/big-bang)

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
```
