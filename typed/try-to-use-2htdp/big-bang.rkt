#lang typed/racket

(provide big-bang BigBang
         (all-from-out "some-2htdp-universe.rkt")
         )

(require typed/2htdp/image
         "some-2htdp-universe.rkt"
         )

(require/typed
 "untyped-big-bang.rkt"
 [big-bang
  (All (World) (BigBang World))])

(define-type (BigBang World)
  (-> World
      [Any -> Boolean : World]
      [#:to-draw (Renderer World)]
      [#:width (U Natural #f)]
      [#:height (U Natural #f)]
      [#:on-tick (TickHandler World)]
      [#:tick-rate Positive-Real]
      [#:tick-limit (U Natural +inf.0)]
      [#:on-key (KeyHandler World)]
      [#:on-release (KeyHandler World)]
      [#:on-pad (PadHandler World)]
      [#:on-mouse (MouseHandler World)]
      [#:stop-when [World -> (U Boolean Image)]]
      [#:record? Any]
      [#:state Boolean]
      [#:register String]
      [#:port Natural]
      [#:on-receive [World Message -> (HandlerResult World)]]
      [#:name (U Symbol String)]
      World))

