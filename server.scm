;;; Procedures for starting and stopping a Gopher server
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   start-server stop-server
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

(define (start-server hostname port router)
  ;; server-ready-mutex is used to check that server is ready to accept
  ;; connections before returning from procedure.
  ;; connects-mutex is used to prevent simultaneous queue operations
  ;; connects-get-cond is used to wait for an item in the connects queue
  (let ((requests (make-queue))
        (connects (make-queue))
        (context (make-context hostname port))
        (server-ready-mutex (make-mutex))
        (connects-mutex (make-mutex))
        (connects-get-cond (make-condition-variable)))

  ;; Add a connect to connects queue
  ;; Thread safety is maintained by connects-mutex and connects-get-cond
  (define (add-connect! connect)
    (mutex-lock! connects-mutex)
    (queue-add! connects connect)
    (condition-variable-signal! connects-get-cond)
    (mutex-unlock! connects-mutex) )

  ;; Returns a connect from connects queue or #f if times out waiting
  ;; for a connect item in queue
  ;; Thread safety is maintained by connects-mutex and connects-get-cond
  (define (get-connect!)
    ;; TODO: What should timeout be, tcp-accept-timeout? or a custom parameter?
    ;; TODO: A longer value than 0.1 would be more efficient but slower for testing
    (let ((timeout 0.1))
      (if (not (mutex-lock! connects-mutex timeout))
          #f
          (if (not (queue-empty? connects))
              (let ((connect (queue-remove! connects)))
                (mutex-unlock! connects-mutex #f)
                connect)
              (begin
                (if (not (mutex-unlock! connects-mutex connects-get-cond timeout))
                    #f
                    (get-connect!) ) ) ) ) ) )

  (define (client-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (log-info "client address: ~A, connection request" client-address)
      (add-connect! (list (cons 'in in) (cons 'out out)
                          (cons 'client-address client-address) ) ) ) )


  ;; Parameter: max-file-size controls the maximum size file
  ;; that can be written
  ;; TODO: Should this log an error/warning if data to write > max-file-size ?
  (define (handle-connect connect)
    (let ((in (alist-ref 'in connect))
          (out (alist-ref 'out connect))
          (client-address (alist-ref 'client-address connect)))
      (let ((selector (read-selector client-address in)))
        ;; TODO: Should this return a timeout error if selector #f?
        (when selector
              (let* ((handler (router-match router selector))
                     (request (make-request selector client-address))
                     (response
                       (if handler
                           (condition-case (handler context request)
                             (ex () (Error-ex ex "exception from handler")))
                           (begin
                             (log-warning "client address: ~A, selector: ~A, no handler for selector"
                                          client-address
                                          selector)
                             (Ok (make-rendered-error-menu context request "path not found"))))))
                (cases Result response
                  (Ok (v) (write-string v (max-file-size) out))
                  (Error (e)
                    (log-error "client address: ~A, selector: ~A, error from handler: ~A"
                               client-address
                               selector
                               e)
                    (write-string (make-rendered-error-menu context request "server error")
                                  (max-file-size)
                                  out)))))
        (close-input-port in)
        (close-output-port out ) ) ) )


  (define (start-connect-handler-thread)
    (let ((connect-handler
            (lambda ()
              (let loop ()
                (handle-exceptions ex
                  (begin
                    (log-error "connect handler thread: ~A"
                               (get-condition-property ex 'exn 'message))
                    (signal ex))
                  (let ((connect (get-connect!)))
                    (when connect
                          (handle-connect connect)))
                  (unless (stop-requested?)
                          (loop)))))))
      (let ((thread (make-thread connect-handler)))
        ;; thread-specific value indicates if thread has been told to stop
        (thread-specific-set! thread #f)
        (thread-start! thread) ) ) )


  ;; Returns a list of threads
  (define (start-connect-handler-threads num-threads)
    (let loop ((n num-threads))
      (if (= n 0)
          '()
          (let ((thread (start-connect-handler-thread)))
            (cons thread (loop (- n 1) ) ) ) ) ) )


  (define (stop-requested?)
    (thread-specific (current-thread) ) )


  (define (stop-connect-handler-threads threads)
    ;; First tell the threads to stop
    (for-each (lambda (thread)
                (thread-specific-set! thread #t))
              threads)
    ;; Then wait for them to finish
    (for-each (lambda (thread)
                (thread-join! thread))
              threads) )


  ;; TODO: Document use of tcp-accept-timeout parameter and possibly change
  ;; TODO: external parameter name to server-accept-timeout, should this
  ;; TODO: change except for testing?  Perhaps have another parameter that
  ;; TODO: is only exported for testing
  ;; Continuously listens to connections to the port and arranges
  ;; for the connections to be handled.  This is designed to run as a
  ;; thread which is stopped by stop-sever.
  (define (listen)
    (parameterize ((tcp-accept-timeout (or (tcp-accept-timeout) 5000)))
      (let ((listener (tcp-listen port)))
        (mutex-unlock! server-ready-mutex)
        (let ((connect-handler-threads (start-connect-handler-threads 10)))
          (let loop ()
            (let-values (((in out) (tcp-accept/handle-timeout listener)))
              (when (and in out)
                    (client-connect in out))
              (if (stop-requested?)
                  (begin
                    (stop-connect-handler-threads connect-handler-threads)
                    (tcp-close listener) )
                  (loop) ) ) ) ) ) ) )


  (let ((thread (make-thread listen)))
    ;; serve-ready-mutex is used to make sure that the procedure doesn't
    ;; return until the server is ready to accept connections.
    ;; thread-specific value indicates if thread has been told to stop
    (thread-specific-set! thread #f)
    (mutex-lock! server-ready-mutex #f #f)
    (thread-start! thread)
    (mutex-lock! server-ready-mutex #f #f)
    thread) ) )


(define (stop-server thread)
  (log-info "stopping server")
  (thread-specific-set! thread #t)
  (thread-join! thread)
  (log-info "server stopped") )



;; Internal Definitions ------------------------------------------------------

;; TODO: Test this including timeout and breaking connection
;; Exceptions, including timeouts, are caught and logged.  If an exception
;; is caught #f is returned
;; Read the selector and trim from the beginning and the end whitespace
;; and '/' characters
(define (read-selector client-address in)
  (condition-case (trim-selector (read-line in 255))
    ((exn i/o net timeout)
      (log-warning "read selector, client address: ~A, read timeout" client-address)
      #f)
    (ex (exn)
      (log-warning "read-selector, client address: ~A, problem reading selector, ~A"
                   client-address
                   (get-condition-property ex 'exn 'message))
      #f)
    (ex ()
      (log-warning "read-selector, client address: ~A, problem reading selector, ~A"
                   client-address
                   (get-condition-property ex 'exn 'message))
      #f) ) )


;; tcp-accept but handles timeouts governed by parameter tcp-accept-timeout
;; Returns accepted connection ports as values: in out
;; or if timeout, values: #f #f
(define (tcp-accept/handle-timeout listener)
  (condition-case (tcp-accept listener)
    ((exn i/o net timeout) (values #f #f) ) ) )

