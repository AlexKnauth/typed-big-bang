#lang typed/racket/base

(provide big-bang BigBang
         KeyEvent key-event? key=?
         PadEvent pad-event? pad=?
         MouseEvent mouse-event? mouse=?
         (struct-out stop-with)
         (struct-out package) make-package
         HandlerResult
         TickHandler
         Renderer
         KeyHandler
         PadHandler
         MouseHandler
         (all-from-out "2htdp/image.rkt")
         )

(require typed/racket/class
         typed/racket/gui
         racket/local
         racket/function
         "2htdp/image.rkt")
(require/typed racket/contract
               [contract-name [(Any -> Boolean) -> Sexp]])
(require/typed mrlib/image-core
               [render-image [Image (Object) Real Real -> Void]])
(require (for-syntax racket/base syntax/parse racket/list))

(define-syntax (.... stx)
  (syntax/loc stx (error "....")))

(define-type Bitmap (Instance Bitmap%))
(define-type Bitmap-DC (Instance Bitmap-DC%))
(define-type Window (Instance Window<%>))
(define-type EventSpace (Opaque eventspace?) #;Eventspace)

(define-type Frame-with-callback (Instance Frame-with-callback%))
(define-type Frame-with-callback%
  (Class #:implements/inits Frame%
         (init-field [char-callback [Window (Instance Key-Event%) -> Any] #:optional]
                     [mouse-callback [Window (Instance Mouse-Event%) -> Any] #:optional]
                     [on-close-callback [-> Any] #:optional])
         ))
(define frame-with-callback% : Frame-with-callback%
  (class frame% (super-new) (inspect #f)
    (init-field [char-callback : [Window (Instance Key-Event%) -> Any] (λ (r e) (void))]
                [mouse-callback : [Window (Instance Mouse-Event%) -> Any] (λ (r e) (void))]
                [on-close-callback : [-> Any] (λ () (void))])
    (define/override (on-subwindow-char reciever event)
      (char-callback reciever event)
      #t)
    (define/override (on-subwindow-event reciever event)
      (mouse-callback reciever event)
      #t)
    (define/augment (on-close)
      (on-close-callback)
      (void))))


(: image->bitmap : [Image -> Bitmap])
(define (image->bitmap image)
  (let*: ([width (assert (image-width image) exact-positive-integer?)]
          [height (assert (image-height image) exact-positive-integer?)]
          [bm : Bitmap (make-bitmap width height)]
          [dc : Bitmap-DC (make-object bitmap-dc% bm)])
    (send dc clear)
    (render-image image dc 0 0)
    bm))



(define one? (make-predicate One))

(: string-with-length-1? : [Any -> Boolean : #:+ String])
(define (string-with-length-1? x)
  (if (string? x)
      (one? (string-length x))
      #f))
(define-type String-With-Length-1 (Opaque string-with-length-1?))

(define-syntax define-1String-type
  (lambda (stx)
    (syntax-parse stx
      [(def 1String)
       #:with (str ...) (map string (map integer->char (range 0 256)))
       #'(define-type 1String (U str ... String-With-Length-1))])))
(define-1String-type 1String)

(define-type KeyEvent
  (U 1String
     "left"
     "right"
     "up"
     "down"
     "start"
     "cancel"
     "clear"
     "shift"
     "rshift"
     "control"
     "rcontrol"
     "menu"
     "pause"
     "capital"
     "prior"
     "next"
     "end"
     "home"
     "escape"
     "select"
     "print"
     "execute"
     "snapshot"
     "insert"
     "help"
     "numpad0" "numpad1" "numpad2" "numpad3" "numpad4" 
     "numpad5" "numpad6" "numpad7" "numpad8" "numpad9" 
     "numpad-enter" "multiply" "add" "separator" "subtract" "decimal" "divide"
     "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12" "f13" 
     "f14" "f15" "f16" "f17" "f18" "f19" "f20" "f21" "f22" "f23" "f24"
     "numlock"
     "scroll"
     "wheel-up"
     "wheel-down"
     "wheel-left"
     "wheel-right"
     ))
(define key-event? (make-predicate KeyEvent))

(: key-event->string : [KeyEvent -> String])
(define (key-event->string key)
  (assert key string?))

(: key=? : [KeyEvent KeyEvent KeyEvent * -> Boolean])
(define (key=? key1 key2 . rst)
  (apply string=? (key-event->string key1) (key-event->string key2) (map key-event->string rst)))

(define-type PadEvent
  (U "left" "right" "up" "down"
     "w" "s" "a" "d"
     " " "shift" "rshift"))
(define pad-event? (make-predicate PadEvent))
(: pad=? (PadEvent PadEvent PadEvent * -> Boolean))
(define pad=? string=?)


(define-type MouseEvent
  (U "button-down" 
     "button-up"
     "drag"
     "move"
     "enter"
     "leave"))
(define mouse-event? (make-predicate MouseEvent))
(: mouse=? (MouseEvent MouseEvent MouseEvent * -> Boolean))
(define mouse=? string=?)

(struct: (HR-World) stop-with ([w : HR-World]) #:transparent)
(struct: (World) package ([w : World] [message : Sexp]) #:transparent)
(: make-package : (All (World) [World Sexp -> (Package World)]))
(define make-package package)

(define-type (HandlerResult World)
  (U (Maybe-Package World)
     (stop-with (Maybe-Package World))))
(define-type (Maybe-Package World)
  (U World (Package World)))
(define-type (Package World)
  (package World))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Key-Code-Symbol
  (U 'start 'cancel 'clear
     'shift ; Shift key
     'rshift ; right Shift key
     'control ; Control key
     'rcontrol ; right Control key
     'menu 'pause 'capital 'prior 'next 'end 'home
     'left 'up 'right 'down
     'escape 'select 'print 'execute 'snapshot 'insert 'help
     'numpad0 'numpad1 'numpad2 'numpad3 'numpad4 'numpad5 'numpad6 'numpad7 'numpad8 'numpad9
     'numpad-enter
     'multiply 'add 'separator 'subtract 'decimal 'divide
     'f1 'f2 'f3 'f4 'f5 'f6 'f7 'f8 'f9 'f10 'f11 'f12 'f13 'f14 'f15 'f16 'f17 'f18 'f19 'f20
     'f21 'f22 'f23 'f24
     'numlock 'scroll
     'wheel-up ; mouse wheel up one notch
     'wheel-down ; mouse wheel down one notch
     'wheel-left ; mouse wheel left one notch
     'wheel-right ; mouse wheel right one notch
     'release ; indicates a key-release event
     'press ; indicates a key-press event; usually only from get-key-release-code
     ;The special key symbols attempt to capture useful keys that have no standard ASCII
     ;representation. A few keys have standard representations that are not obvious:
     #\space ; the space bar
     #\return ; the Enter or Return key (on all platforms), but not necessarily the Enter key near the
     ;numpad (which is reported as 'numpad-enter if the platform distinguishes the two Enter keys)
     #\tab ; the tab key
     #\backspace ; the backspace key
     #\rubout ; the delete key
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: key-event%->KeyEvent : [(Instance Key-Event%) -> KeyEvent])
(define (key-event%->KeyEvent event)
  (let ([key-code (cast (send event get-key-code) (U Char Key-Code-Symbol))]
        [key-release-code (cast (send event get-key-release-code) (U Char Key-Code-Symbol))])
    (cond [(char? key-code) (assert (string key-code) key-event?)]
          [(equal? 'release key-code)
           (cond [(char? key-release-code) (assert (string key-release-code) key-event?)]
                 [(symbol? key-release-code) (assert (symbol->string key-release-code) key-event?)])]
          [(symbol? key-code) (assert (symbol->string key-code) key-event?)])))

(: mouse-event%->MouseEvent : [(Instance Mouse-Event%) -> MouseEvent])
(define (mouse-event%->MouseEvent event)
  (cond [(send event button-down? 'any) "button-down"]
        [(send event button-up? 'any) "button-up"]
        [(send event entering?) "enter"]
        [(send event leaving?) "leave"]
        [(send event dragging?) "drag"]
        [(send event moving?) "move"]
        [else (error 'mouse-event%->MouseEvent
                     (string-append
                      "mouse-event%->MouseEvent: don't know what to do. " "\n"
                      "  given: ~v"#;(~v event)"" "\n"
                      "  event-type: ~v"#;(~v (send event get-event-type))"")
                     event
                     (send event get-event-type))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type (TickHandler World)
  [World -> (HandlerResult World)])

(define-type (Renderer World)
  [World -> Image])

(define-type (KeyHandler World)
  [World KeyEvent -> (HandlerResult World)])

(define-type (PadHandler World)
  [World PadEvent -> (HandlerResult World)])

(define-type (MouseHandler World)
  [World Integer Integer MouseEvent -> (HandlerResult World)])

(define-type (BigBang World)
  (-> (U World (stop-with World))
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
      [#:stop-when (World -> (U Boolean Image))]
      ;[#:record? Any]
      ;[#:state Boolean]
      ;[#:register String]
      ;[#:on-recieve (World Message -> (HandlerResult World))]
      [#:name (U Symbol String)]
      [#:on-key/key-event% [World (Instance Key-Event%) -> (HandlerResult World)]]
      [#:on-mouse/mouse-event% [World (Instance Mouse-Event%) -> (HandlerResult World)]]
      World))

(: default-key-handler : (All (World) (KeyHandler World)))
(define (default-key-handler world key) world)

(: default-mouse-handler : (All (World) (MouseHandler World)))
(define (default-mouse-handler world x y mouse-event) world)

(: make-key-event%-handler :
   (All (World) [-> (KeyHandler World) (KeyHandler World) (U (PadHandler World) #f)
                    [World (Instance Key-Event%) -> (HandlerResult World)]]))
(define (make-key-event%-handler handle-key handle-release handle-pad)
  (: handle-key-event% : [World (Instance Key-Event%) -> (HandlerResult World)])
  (define (handle-key-event% world event)
    (define key (key-event%->KeyEvent event))
    (cond [(and handle-pad (pad-event? key)) (handle-pad world key)]
          [(equal? 'release (send event get-key-code)) (handle-release world key)]
          [else (handle-key world key)]))
  handle-key-event%)

(: make-mouse-event%-handler :
   (All (World) [-> (MouseHandler World) [World (Instance Mouse-Event%) -> (HandlerResult World)]]))
(define (make-mouse-event%-handler handle-mouse)
  (: handle-mouse-event% : [World (Instance Mouse-Event%) -> (HandlerResult World)])
  (define (handle-mouse-event% world event)
    (handle-mouse world (send event get-x) (send event get-y)
                  (mouse-event%->MouseEvent event)))
  handle-mouse-event%)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(: big-bang : (All (World) (BigBang World)))
(define
  (big-bang world
            world?
            #:to-draw [render (λ (world) (empty-scene 100 100))]
            #:width [width #f]
            #:height [height #f]
            #:on-tick [tick identity]
            #:tick-rate [tick-rate 30]
            #:tick-limit [tick-limit +inf.0]
            #:on-key [handle-key (inst default-key-handler World)]
            #:on-release [handle-release default-key-handler]
            #:on-pad [handle-pad #f]
            #:on-mouse [handle-mouse (inst default-mouse-handler World)]
            #:stop-when [last-world? (λ: ([world : World]) #f)]
            ;#:record? [record? #f]
            ;#:state [state #f]
            ;#:register [IP-expr #f]
            ;#:on-recieve [handle-recieve (λ (world message) world)]
            #:name [name "World"]
            #:on-key/key-event%
            [handle-key/key-event%
             ((inst make-key-event%-handler World) handle-key handle-release handle-pad)]
            #:on-mouse/mouse-event%
            [handle-mouse/mouse-event%
             ((inst make-mouse-event%-handler World) handle-mouse)]
            )
  (local [(: check-world : [Any -> World])
          (define (check-world world)
            (cond [(world? world) world]
                  [else (error 'typed-big-bang "expected ~v, but recieved ~v"
                               (contract-name world?) world)]))
          (: current-world : (case-> [-> (U World (stop-with World))]
                                     [(U World (stop-with World)) -> Void]))
          (define current-world
            (case-lambda
              [() world]
              [(w) (set! world w)]))]
    (define first-img  (render (check-world world)))
    (define eventspace (make-eventspace))
    (define frame-width : Natural
      (if width
          (max width (image-width first-img))
          (max 0     (image-width first-img))))
    (define frame-height : Natural
      (if height
          (max height (image-height first-img))
          (max 0      (image-height first-img))))
    (define frame : Frame-with-callback
      (let ([label : String (cond [(string? name) name] [(symbol? name) (symbol->string name)])])
        (parameterize ([current-eventspace eventspace])
          (new frame-with-callback%
               [label label]
               [width frame-width]
               [height frame-height]
               [char-callback
                (λ (window event)
                  (let ([world  (current-world)])
                    (cond [(stop-with? world) (void)]
                          [else (define result (handle-key/key-event% world event))
                                (current-world (handle-handler-result result world?))])))]
               [mouse-callback
                (λ (window event)
                  (let ([world (current-world)])
                    (cond [(stop-with? world) (void)]
                          [else (define result (handle-mouse/mouse-event% world event))
                                (current-world (handle-handler-result result world?))])))]
               [on-close-callback
                (λ ()
                  (let ([world (current-world)])
                    (cond [(stop-with? world) (void)]
                          [else (current-world (stop-with world))])))]
               ))))
    (define message : (Instance Message%)
      (new message% [parent frame] [label (image->bitmap first-img)]))
    (: loop : [#:tick-limit (U Natural +inf.0) -> World])
    (define (loop #:tick-limit tick-limit)
      (local [(define start-time (current-inexact-milliseconds))
              (define (elapsed-milliseconds)
                (- (current-inexact-milliseconds) start-time))
              (define (elapsed-seconds)
                (* 0.001 (elapsed-milliseconds)))
              (define (wait)
                (sleep (max 0 (- (/ tick-rate) (elapsed-seconds)))))
              (: maybe-recur : [(U World (stop-with World)) -> World])
              (define (maybe-recur hr)
                (cond [(stop-with? hr) (define last-world (check-world (stop-with-w hr)))
                                       (define last-scene (render last-world))
                                       (send message set-label (image->bitmap last-scene))
                                       last-world]
                      [else (current-world hr)
                            (recur)]))
              (: recur : [-> World])
              (define (recur)
                (wait) (loop #:tick-limit (cast (sub1 tick-limit) (U Natural +inf.0))))]
        (let ([world (current-world)])
          (cond [(<= tick-limit 0) (define last-world (if (stop-with? world)
                                                          (check-world (stop-with-w world))
                                                          world))
                                   (define last-scene (render last-world))
                                   (send message set-label (image->bitmap last-scene))
                                   last-world]
                [(stop-with? world) (define last-world (check-world (stop-with-w world)))
                                    (define last-scene (render last-world))
                                    (send message set-label (image->bitmap last-scene))
                                    last-world]
                [(last-world? world) => (lambda (maybe-last-scene)
                                          (define last-scene
                                            (if (image? maybe-last-scene)
                                                maybe-last-scene
                                                (render world)))
                                          (send message set-label (image->bitmap last-scene))
                                          world)]
                [else
                 (define img (render world))
                 (send message set-label (image->bitmap img))
                 (define next-world (handle-handler-result (tick world) world?))
                 (maybe-recur next-world)]
                )
          )))
    (send frame show #t)
    (loop #:tick-limit tick-limit)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(: handle-handler-result :
   (All (World)
        (case-> [(Package World) [Any -> Boolean : World] -> World]
                [(HandlerResult World) [Any -> Boolean : World] -> (U World (stop-with World))])))
(define (handle-handler-result pkg world?)
  (define hdl-pkg (inst handle-package World))
  (cond [(world? pkg) pkg]
        [(package? pkg) (hdl-pkg (ann pkg (Package World)))]
        [(stop-with? pkg) ((inst stop-with World)
                           (hdl-pkg
                            ((inst stop-with-w (U World (Package World)))
                             pkg)))]
        [else pkg]))

(: handle-package : (All (World) [(U World (Package World)) -> World]))
(define (handle-package pkg)
  (cond [(package? pkg) ....]
        [else pkg]))

