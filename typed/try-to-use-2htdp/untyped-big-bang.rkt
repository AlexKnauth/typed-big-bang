#lang racket/base

(provide big-bang)

(require (only-in
          2htdp/universe
          [big-bang 2htdp:big-bang]
          check-with
          to-draw
          on-tick
          on-key
          on-release
          on-pad
          on-mouse
          stop-when
          record?
          state
          register
          port
          on-receive
          name
          )
         2htdp/image
         racket/function
         racket/promise
         )

(define (default-key-handler world key) world)

(define (default-mouse-handler world x y mouse-event) world)

(define
  (big-bang world
            world?
            #:to-draw [render (λ (world) (empty-scene 100 100))]
            #:width [width #f]
            #:height [height #f]
            #:on-tick [tick identity]
            #:tick-rate [tick-rate 30]
            #:tick-limit [tick-limit +inf.0]
            #:on-key [handle-key default-key-handler]
            #:on-release [handle-release default-key-handler]
            #:on-pad [handle-pad handle-key]
            #:on-mouse [handle-mouse default-mouse-handler]
            #:stop-when [last-world? (λ (world) #f)]
            #:record? [-record? #f]
            #:state [-state #f]
            #:register [IP #f]
            #:port [-port #f]
            #:on-receive [handle-receive (λ (world message) world)]
            #:name [-name "World"])
  (let* ([first-scene (delay (render world))]
         [width (or width (image-width (force first-scene)))]
         [height (or height (image-height (force first-scene)))])
    (cond [IP (cond [(equal? tick-limit +inf.0)
                     (2htdp:big-bang world
                                     [check-with world?]
                                     [to-draw render width height]
                                     [on-tick tick tick-rate]
                                     [on-key handle-key]
                                     [on-release handle-release]
                                     [on-pad handle-pad]
                                     [on-mouse handle-mouse]
                                     [stop-when last-world?]
                                     [record? -record?]
                                     [state -state]
                                     [register IP]
                                     [port -port]
                                     [on-receive handle-receive]
                                     [name -name])]
                    [else
                     (2htdp:big-bang world
                                     [check-with world?]
                                     [to-draw render width height]
                                     [on-tick tick tick-rate tick-limit]
                                     [on-key handle-key]
                                     [on-release handle-release]
                                     [on-pad handle-pad]
                                     [on-mouse handle-mouse]
                                     [stop-when last-world?]
                                     [record? -record?]
                                     [state -state]
                                     [register IP]
                                     [port -port]
                                     [on-receive handle-receive]
                                     [name -name])])]
          [else (cond [(equal? tick-limit +inf.0)
                       (2htdp:big-bang world
                                       [check-with world?]
                                       [to-draw render width height]
                                       [on-tick tick tick-rate]
                                       [on-key handle-key]
                                       [on-release handle-release]
                                       [on-pad handle-pad]
                                       [on-mouse handle-mouse]
                                       [stop-when last-world?]
                                       [record? -record?]
                                       [state -state]
                                       [name -name])]
                      [else
                       (2htdp:big-bang world
                                       [check-with world?]
                                       [to-draw render width height]
                                       [on-tick tick tick-rate tick-limit]
                                       [on-key handle-key]
                                       [on-release handle-release]
                                       [on-pad handle-pad]
                                       [on-mouse handle-mouse]
                                       [stop-when last-world?]
                                       [record? -record?]
                                       [state -state]
                                       [name -name])])])))


