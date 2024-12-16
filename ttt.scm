(use-modules (scheme write)
             (hoot ffi)
             (fibers)
             (fibers promises)
             (fibers timers))


(define-foreign console-log
  "window" "log"
  (ref string) -> (ref null extern))

(console-log "Before promise!")

(lambda (resolved rejected)
  (call-with-async-result
   resolved rejected
   (lambda ()
     (console-log "entered promise..."))))

(console-log "After promise!")
