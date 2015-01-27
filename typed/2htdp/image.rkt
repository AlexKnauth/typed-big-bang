#lang typed/racket/base

(require/typed/provide
 2htdp/image
 [#:opaque Image image?]
 [image-width [Image -> Natural]]
 [image-height [Image -> Natural]]
 [empty-scene [Natural Natural -> Image]]
 [overlay [Image Image * -> Image]]
 [text [String Natural String -> Image]]
 )

