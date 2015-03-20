#lang typed/racket/base

(provide image->bitmap)

(require typed/racket/class
         (only-in typed/racket/draw Bitmap% make-bitmap bitmap-dc%)
         (only-in typed/2htdp/image Image image-width image-height))
(require/typed mrlib/image-core
               [render-image [Image (Object) Real Real -> Void]])

(: image->bitmap : Image -> (Instance Bitmap%))
(define (image->bitmap image)
  (let* ([width (assert (image-width image) positive?)]
         [height (assert (image-height image) positive?)]
         [bm (make-bitmap width height)]
         [dc (make-object bitmap-dc% bm)])
    (send dc clear)
    (render-image image dc 0 0)
    bm))

