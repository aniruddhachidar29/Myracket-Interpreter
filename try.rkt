#lang racket
(require racket/struct)
(require "defs.rkt")
(require "examples.rkt")
(require "model-interpreter.rkt")



(displayln "Program 11********************************\n")
;
(displayln prog11)
;
(displayln "\n\nProgram evaluation ********************************\n")

(eval-program prog11)

;;and so on 