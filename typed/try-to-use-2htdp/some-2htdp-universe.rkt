#lang typed/racket

(provide KeyEvent key-event? key=?
         PadEvent pad-event? pad=?
         MouseEvent mouse-event? mouse=?
         Package package? make-package
         Message sexp?
         StopWith stop-with stop-with? stop-with-w
         HandlerResult
         TickHandler
         Renderer
         KeyHandler
         PadHandler
         MouseHandler
         )

(require typed/2htdp/image
         "1String.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     ))

(require/typed
 2htdp/universe
 [#:opaque -KeyEvent key-event?]
 [#:opaque -PadEvent pad-event?]
 [#:opaque -MouseEvent mouse-event?]
 [#:opaque Package package?]
 [#:opaque -Message sexp?]
 [key=? [KeyEvent KeyEvent -> Boolean]]
 [pad=? [PadEvent PadEvent -> Boolean]]
 [mouse=? [MouseEvent MouseEvent -> Boolean]]
 [make-package [Any Message -> Package]]
 [stop-with [Any -> StopWith]]
 )
(require/typed
 2htdp/private/stop
 [#:struct stop-the-world ([world : Any])])

(: stop-with? : [Any -> Boolean : StopWith])
(define stop-with? stop-the-world?)

(: stop-with-w : [StopWith -> Any])
(define stop-with-w stop-the-world-world)

(define-type StopWith stop-the-world)

(define-type (HandlerResult World)
  (U World StopWith Package))

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

(define-type Message
  (U -Message Any))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type KeyEvent
  (U -KeyEvent
     1String
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

(define-type PadEvent
  (U -PadEvent
     "left" "right" "up" "down"
     "w" "s" "a" "d"
     " " "shift" "rshift"))

(define-type MouseEvent
  (U -MouseEvent
     "button-down" 
     "button-up"
     "drag"
     "move"
     "enter"
     "leave"))

