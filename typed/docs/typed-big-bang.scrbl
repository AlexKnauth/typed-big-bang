#lang scribble/manual

@(require (for-label
           typed/big-bang
           typed/2htdp/image
           (only-meta-in 0 typed/racket)
           ))

@title{typed-big-bang}

source code: @url["https://github.com/AlexKnauth/typed-big-bang"]

@section{typed/big-bang}

@defmodule[typed/big-bang]

@defproc[
(big-bang [init-state World]
          [world? [Any -> Boolean : World]]
          [#:on-tick tick (TickHandler World) identity]
          [#:tick-rate rate Positive-Real 30]
          [#:tick-limit limit (U Natural +inf.0) +inf.0]
          [#:on-key handle-key (KeyHandler World) (λ ....)]
          [#:on-pad handle-pad (PadHandler World) #f]
          [#:on-release handle-release (KeyHandler World) (λ ....)]
          [#:on-mouse handle-mouse (MouseHandler World) (λ ....)]
          [#:to-draw render (Renderer World) (λ ....)]
          [#:width width (U Natural #f) #f]
          [#:height height (U Natural #f) #f]
          [#:stop-when last-world? [World -> (U Boolean Image)] (λ ....)]
          [#:name name (U Symbol String) "World"]
          [#:on-key/key-event% handle-key-event%
                               [World (Instance Key-Event%) -> (HandlerResult World)]
                               (λ ....)]
          [#:on-mouse/mouse-event% handle-mouse-event%
                                   [World (Instance Mouse-Event%) -> (HandlerResult World)]
                                   (λ ....)]) World]{
a function that bahaves like big-bang from @racketmodname[2htdp/universe].

An example world program:
@racketmod[typed/racket

(require typed/big-bang
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
]}

@defform[#:kind "type" (TickHandler World)]{
equivalent to @racket[[World -> (HandlerResult World)]]}

@defform[#:kind "type" (Renderer World)]{
equivalent to @racket[[World -> Image]]}

@defform[#:kind "type" (KeyHandler World)]{
equivalent to @racket[[World KeyEvent -> (HandlerResult World)]]}

@defform[#:kind "type" (PadHandler World)]{
equivalent to @racket[[World PadEvent -> (HandlerResult World)]]}

@defform[#:kind "type" (MouseHandler World)]{
equivalent to @racket[[World Integer Integer MouseEvent -> (HandlerResult World)]]}

@defform[#:kind "type" (HandlerResult World)]{
equivalent to @racket[(U World StopWith)]}

@defidform[#:kind "type" KeyEvent]{
the type for a key event, corrosponding to KeyEvents from @racketmodname[2htdp/universe]}

@defidform[#:kind "type" MouseEvent]{
the type for a mouse event, corrosponding to MouseEvents from @racketmodname[2htdp/universe]}

@defidform[#:kind "type" PadEvent]{
the type for a pad event, corrosponding to PadEvents from @racketmodname[2htdp/universe]}

@deftogether[[
@defidform[#:kind "type" StopWith]
@defstruct*[stop-with ([w World])]]]{
a struct to tell big bang to stop the world with the final state @racket[w].}

@defproc[(key-event? [v Any]) Boolean]{
returns @racket[#t] if @racket[v] is a @racket[KeyEvent].}

@defproc[(key=? [x KeyEvent] [y KeyEvent] ...) Boolean]{
returns @racket[#t] if the given @racket[KeyEvent]s are equal.}

@defproc[(pad-event? [v Any]) Boolear]{
returns @racket[#t] if @racket[v] is a @racket[PadEvent].}

@defproc[(pad=? [x PadEvent] [y PadEvent] ...) Boolean]{
returns @racket[#t] if the given @racket[PadEvent]s are equal.}

@defproc[(mouse-event? [v Any]) Boolean]{
returns @racket[#t] if @racket[v] is a @racket[MouseEvent].}

@defproc[(mouse=? [x MouseEvent] [y MouseEvent] ...) Boolean]{
returns @racket[#t] if the given @racket[MouseEvent]s are equal.}

@section{typed/2htdp/image}

@defmodule[typed/2htdp/image]{
provides types for some functions from @racketmodname[2htdp/image].
}

@defidform[#:kind "type" Image]{
the type for images from @racketmodname[2htdp/image]}

@defidform[#:kind "type" Image-Color]{a color}

@defproc[(image? [v Any]) Boolean]{
returns @racket[#t] if @racket[v] is an @racket[Image]}

@defproc[(image-width [img Image]) Natural]{returns the width of @racket[img]}
@defproc[(image-height [img Image]) Natural]{returns the height of @racket[img]}
@defproc[(empty-scene [width Natural] [height Natural]) Image]{
returns an empty scene with the given @racket[width] and @racket[height]}
@defproc[(overlay [i1 Image] [i2 Image] ...) Image]{
overlays @racket[i1] on top of @racket[i2], and so on.}
@defproc[(text [str String] [size Natural] [color Image-Color]) Image]{
returns an image that draws @racket[str].}


