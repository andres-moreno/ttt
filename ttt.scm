(use-modules (scheme write)
             (hoot ffi)
             (ice-9 match)
             (srfi srfi-9)
             (fibers)
             (ice-9 atomic)
             (fibers channels)
             (fibers conditions)
             (fibers operations)
             (fibers promises)
             (fibers timers))

(define-foreign console-log
  "core" "log"
  (ref string) -> (ref null extern))

;; (ice-9 q)

(define (sync-q! q)
  (set-cdr! q (if (pair? (car q)) (last-pair (car q))
		  #f))
  q)

;;; make-q
;;;  return a new q.
;;;
(define (make-q) (cons '() #f))

;;; q? obj
;;;   Return true if obj is a Q.
;;;   An object is a queue if it is equal? to '(() . #f)
;;;   or it is a pair P with (list? (car P))
;;;                      and (eq? (cdr P) (last-pair (car P))).
;;;
(define (q? obj)
  (and (pair? obj)
       (if (pair? (car obj))
	   (eq? (cdr obj) (last-pair (car obj)))
	   (and (null? (car obj))
		(not (cdr obj))))))

;;; q-empty? obj
;;;
(define (q-empty? obj) (null? (car obj)))

;;; q-empty-check q
;;;  Throw a q-empty exception if Q is empty.
(define (q-empty-check q) (if (q-empty? q) (throw 'q-empty q)))

;;; q-front q
;;;  Return the first element of Q.
(define (q-front q) (q-empty-check q) (caar q))

;;; q-rear q
;;;  Return the last element of Q.
(define (q-rear q) (q-empty-check q) (cadr q))

;;; q-remove! q obj
;;;  Remove all occurences of obj from Q.
(define (q-remove! q obj)
  (set-car! q (delq! obj (car q)))
  (sync-q! q))

;;; q-push! q obj
;;;  Add obj to the front of Q
(define (q-push! q obj)
  (let ((h (cons obj (car q))))
    (set-car! q h)
    (or (cdr q) (set-cdr! q h)))
  q)

;;; enq! q obj
;;;  Add obj to the rear of Q
(define (enq! q obj)
  (let ((h (cons obj '())))
    (if (null? (car q))
	(set-car! q h)
	(set-cdr! (cdr q) h))
    (set-cdr! q h))
  q)

;;; q-pop! q
;;;  Take the front of Q and return it.
(define (q-pop! q)
  (q-empty-check q)
  (let ((it (caar q))
	(next (cdar q)))
    (if (null? next)
	(set-cdr! q #f))
    (set-car! q next)
    it))

;;; deq! q
;;;  Take the front of Q and return it.
(define deq! q-pop!)

;;; q-length q
;;;  Return the number of enqueued elements.
;;;
(define (q-length q) (length (car q)))

;; ----------------------------------------------------------------------------
;; Inbox code from Spritely
;;
;; Modified to take a procedure with arguments
;;   - enq-ch: enqueue channel
;;   - deq-ch: dequeue channel
;;   - stop-op: procedure to stop the loop 
(define* (inbox code)
  (define enq-ch (make-channel))        ; inbound messages
  (define deq-ch (make-channel))        ; outbound messages
  (define stop? (make-condition))
  (define back-queue (make-q))
  (define next-one #f)
  (define (start-inbox-loop)
    (define keep-going? #t)
    ;; Incoming
    (define (enq-op)
      (wrap-operation (get-operation enq-ch)
                      (lambda (msg)
                        (if next-one
                            (enq! back-queue msg)
                            (set! next-one msg)))))
    ;; outgoing
    (define (deq-op)
      (wrap-operation
       ;; send it...
       (put-operation deq-ch next-one)
       ;; and pull next-one off the back-queue, if appropriate
       ;; (or indicate that there *is* no next-one...)
       (lambda _
         (if (q-empty? back-queue)
             (set! next-one #f)
             (set! next-one (deq! back-queue))))))
    (define (stop-op)
      (wrap-operation (wait-operation stop?)
                      (lambda () (set! keep-going? #f))))

    (lambda ()
      (let forever ()
        (if  keep-going?
             (begin
               (perform-operation
                (if next-one
                    (choice-operation (deq-op) (enq-op) (stop-op))
                    (choice-operation (enq-op) (stop-op))))
               (forever))
             ;; will fire when the condition variable stop? is set
             (display "forever loop finished!\n")))))

 
  ;; boot it up!
  (spawn-fiber (start-inbox-loop))

  ;; execute our code using the channels defined above
  (code enq-ch deq-ch stop?))

(define (my-threads enq-ch deq-ch stop?)

  ;; fiber errors out if I use console-log instead of display
  (spawn-fiber (lambda ()
                 (let ((msg (get-message deq-ch)))
                   (console-log msg)
                   )))
  (spawn-fiber (lambda ()
                 (put-message enq-ch 'move)))
  (spawn-fiber (lambda ()
                 (put-message enq-ch 'history)))
  (spawn-fiber (lambda ()
                 (put-message enq-ch 'move)))
  (spawn-fiber (lambda ()
                 (let ((msg (get-message deq-ch)))
                   (display msg)
                   (newline)
                   (force-output))))
  (spawn-fiber (lambda ()
                 (let ((msg (get-message deq-ch)))
                   (display msg)
                   (newline)
                   (force-output))))
  (spawn-fiber (lambda ()
                 (signal-condition! stop?))))


(lambda (resolved rejected)
  (call-with-async-result
   resolved rejected
   (lambda ()
     ;; note: no issue with console-log here
     (console-log "Starting the promise code...\n")
     ;; code running with Spritely Inbox queues
     (inbox my-threads)
     ;; give fibers a chance to finish.
     (sleep 0.1)
     (console-log "done with promise code!\n")
     )))


