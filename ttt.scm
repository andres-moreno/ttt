(use-modules (scheme write)
             (hoot ffi)
             (fibers)
             (fibers promises))

(define-foreign console-log
  "core" "log"
  (ref string) -> (ref null extern))

(console-log "Before promise!")

(lambda (resolved rejected)
  (call-with-async-result
   resolved rejected
   (lambda ()
     (console-log "entered promise...")
     42)))
