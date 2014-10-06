#lang racket/base
(require 2htdp/universe
         (only-in racket/gui/base
                  current-eventspace
                  make-eventspace
                  queue-callback))

(provide big-bang/big-crunch)

(define (call-with-big-crunch thunk)
  (define clean-up (make-custodian))
  (define ch (make-channel))
  (parameterize ([current-custodian clean-up])
    (parameterize ([current-eventspace (make-eventspace)])
      (queue-callback
       (lambda ()
         (channel-put ch (thunk))))))
  (begin0
   (channel-get ch)
   (custodian-shutdown-all clean-up)))

(define-syntax-rule (big-bang/big-crunch state clause ...)
  (call-with-big-crunch
   (lambda ()
     (big-bang state clause ...))))
