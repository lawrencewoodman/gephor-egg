;;; A Gopher server module for Chicken Scheme
;;;
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

;; TODO: rename
(module gophser
  (start-server make-router router-add router-match)

(import scheme
        (chicken base)
        (chicken condition)
        (chicken format)
        (chicken io)
        (chicken port)
        (chicken process signal)
        (chicken sort)
        (chicken string)
        (chicken tcp)
        (chicken type)
        queues
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-18 - Multithreading support
;; queues  - In the source code it says that the procedures used
;;           here are thread safe



(define (start-server port router)
  (let ((requests (make-queue))
        (context (list (cons 'port port))))

  (define (termination-handler signum)
    ;; TODO: What else should this do
    ;; TODO: What message should be reported?
    ;; TODO: Should the message go to stderr and could it be better?
    ;; TODO: How would we handle this if this becomes a module rather than a
    ;; TODO: program?
    (write-line "server terminated")
    (exit))
    
  ;; TODO: add timeout
  (define (read-selector in)
    (read-line in 255) )


  (define (client-connect in out)
    (printf "waiting for selector~%~!")
    (let ([selector (read-selector in)]) 
      (printf "got selector: ~A~%~!" selector)
      (let-values ([(_ client-address) (tcp-addresses in)])
        (printf "client address: ~A, selector: ~A~%~!" client-address selector)
        ;; TODO: Does in need to be in this and could it be closed first?
        ;; TODO: Find  a neater way of doing this
        (queue-add! requests (list (cons 'in in) (cons 'out out) 
                                   (cons 'client-address client-address)
                                   (cons 'selector selector) ) ) ) ) )


  ;; TODO: Need a way to handle different types of response: binary, text, etc
  ;; TODO: Need to handle handlers failing
  (define (handle-request request)
    (let* ((selector (cdr (assoc 'selector request)))
           (in (cdr (assoc 'in request)))
           (out (cdr (assoc 'out request)))
           (client-address (cdr (assoc 'client-address request)))
           (handler (router-match router selector)))
      (write-line (sprintf "hello client: ~A, asking for ~A~!" client-address selector) out) 
      (if handler
          (let ((response (handler context request)))
            (write-string response (string-length response) out))
          (write-line (sprintf "selector not found") out))  ; TODO: return an error gophermap
      ;; TODO: When should these be closed?
      (close-input-port in)
      (close-output-port out) ) )
    

  ;; TODO: should this end in a !
  ;; TODO: should we have a way to tell thread to stop cleanly
  ;; TODO: Rename
  (define (requests-handler-thread-start)
    (thread-start! 
      (make-thread 
        (lambda ()
          (let loop ()
            (handle-exceptions ex
              (signal ex)
              (if (not (queue-empty? requests))
                  (let* ((request (queue-remove! requests)))
                    (handle-request request) )
                  ;; The thread-sleep! is to prevent the server from
                  ;; continually polling when the requests queue is empty
                  ;; TODO: Find a better way of doing this perhaps using a mutex
                  ;; TODO: and altering it on client-connect
                  (thread-sleep! 0.1) ) )
            (loop) ) ) ) ) )


  ;; TODO: should we also set on-exit to do something?
  (define (set-termination-handler)
    (set-signal-handler! signal/hup termination-handler)
    (set-signal-handler! signal/int termination-handler)
    (set-signal-handler! signal/term termination-handler) )


  (let ((listener (tcp-listen port)))
    (set-termination-handler)
    (requests-handler-thread-start)
    (let loop ()
      (let-values ([(in out) (tcp-accept listener)])
        (client-connect in out))
      (loop) ) ) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Router
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Create a types file
(define-type router (list-of (pair string procedure)))


;; Exported Definitions ------------------------------------------------------

;; TODO: Add args to type def
;; TODO: typedef procedure?
(: make-router ((list-of (pair string procedure)) --> router))
(define (make-router . args)
  (router-sort-routes args))


;; TODO: check the pattern doesn't already exist
;; TODO: error if multiple * or * not at end
(: router-add (router string procedure --> router))
(define (router-add router pattern proc)
  (let ((route (cons pattern proc)))
    ; Add the route to the list of routes and resort
    (router-sort-routes (cons route router) ) ) )


;; TODO: should this return the pattern as well, perhaps using values?
;; TODO: define the procedure argument types?
(: router-match (router string --> (or procedure boolean)))
(define (router-match router selector)
  (let loop ((routes router))
    (and (not (null? routes))
         (let* ((route (car routes))
                (pattern (car route))
                (proc (cdr route))
                (splat-index (substring-index "*" pattern)))
          (if splat-index
              (if (substring=? selector pattern 0 0 splat-index)
                  proc
                  (loop (cdr routes)))
              (if (equal? selector pattern)
                  proc
                  (loop (cdr routes) ) ) ) ) ) ) )
                   


;; Internal Definitions ------------------------------------------------------

;; Sort routes in descending order of length of string before '*'
;; so that most general matches are at the end of the list.  If two
;; patterns are the same length sort in lexical order
;; TODO: rewrite explanation as it isn't quite right
(: router-sort-routes (router --> router))
(define (router-sort-routes router)
  (sort router
        (lambda (a b)
          (let ((a-pat (car a))
                (b-pat (car b)))
           (let ((splat-index-a (substring-index "*" a-pat))
                 (splat-index-b (substring-index "*" b-pat)))
             (if splat-index-a
                 (if (not splat-index-b)
                     #f
                     (if (= splat-index-a splat-index-b)
                         (string<? a-pat b-pat)
                         (< splat-index-b splat-index-a)))
                 (if splat-index-b
                     #t
                     (string<? a-pat b-pat) ) ) ) ) ) ) )

)

