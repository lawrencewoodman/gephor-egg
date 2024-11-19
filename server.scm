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


;; Start the gopher server in a separate thread.
;; Returns a thread which can be passed to stop-server to stop the server.
;;
;; Uses the paramaters tcp-accept-timeout and tcp-read-timeout to control
;; timeouts.
(define (start-server #!key (hostname (server-hostname))
                            (port (server-port))
                            router)
  ;; server-ready-mutex is used to check that server is ready to accept
  ;; connections before returning from procedure.
  ;; connections-mutex is used to prevent simultaneous changing of connection count
  ;; max-number-of-connections specifies how many connections can be handled at one time
  ;; number-of-connections is the current number of connections to the server
  (let ((server-ready-mutex (make-mutex))
        (connections-mutex (make-mutex))
        (max-number-of-connections 50)   ;; TODO: parameterize this?
        (number-of-connections 0))

  (define (inc-connection-count)
    (mutex-lock! connections-mutex)
    (set! number-of-connections (add1 number-of-connections))
    (mutex-unlock! connections-mutex) )

  (define (dec-connection-count)
    (mutex-lock! connections-mutex)
    (set! number-of-connections (sub1 number-of-connections))
    (mutex-unlock! connections-mutex) )

  ;; Parameter: max-file-size controls the maximum size file
  ;; that can be written
  (define (handle-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (let ((selector (read-selector client-address in)))
        (when selector
              (let* ((handler (router-match router selector))
                     (request (make-request selector client-address))
                     (response
                       (and handler
                           (condition-case (handler request)
                             (ex ()
                               (log-error "client address: ~A, selector: ~A, exception raised by handler: ~A"
                                          client-address
                                          selector
                                          (get-condition-property ex 'exn 'message))
                               (make-rendered-error-menu request "resource unavailable"))))))
                (if response
                    (if (> (string-length response) (max-file-size))
                        (begin
                          (log-error "client address: ~A, selector: ~A, data is too big to send"
                                     client-address
                                     selector)
                          (write-string (make-rendered-error-menu request "resource is too big to send")
                                        (max-file-size)
                                        out))
                        (write-string response (max-file-size) out))
                    (begin
                      (log-warning "client address: ~A, selector: ~A, no handler for selector"
                                   client-address
                                   selector)
                      (write-string (make-rendered-error-menu request "path not found")
                                    (max-file-size)
                                    out)))))
        (close-input-port in)
        (close-output-port out ) ) ) )

  (define (start-connect-thread in out)
    (thread-start!
      (make-thread
        (lambda ()
          (inc-connection-count)
          (handle-exceptions ex
            (begin
              (dec-connection-count)
              ;; TODO: Improve error message
              (log-error "connect handler thread: ~A"
                         (get-condition-property ex 'exn 'message))
              (signal ex))     ;; TODO: signal?
            (handle-connect in out)
            (dec-connection-count))))))

  (define (wait-for-connections-to-finish)
    (let loop ()
      (mutex-lock! connections-mutex)
      (let ((count number-of-connections))
        (mutex-unlock! connections-mutex)
        (unless (= count 0)
                (loop) ) ) ) )

  ;; Continuously listens to connections to the port and arranges
  ;; for the connections to be handled.  This is designed to run as a
  ;; thread which is stopped by stop-sever.
  (define (listen)
    (parameterize ((tcp-accept-timeout (or (tcp-accept-timeout) 5000)))
      (let ((listener (tcp-listen port)))
        (mutex-unlock! server-ready-mutex)
        (let loop ()
          (if (>= number-of-connections max-number-of-connections)
              (log-info "thread limit reached: ~A" number-of-connections)
              (let-values (((in out) (tcp-accept/handle-timeout listener)))
                (when (and in out)
                      (start-connect-thread in out))
                (unless (stop-requested?)
                        (loop)))))
        (wait-for-connections-to-finish)
        (tcp-close listener) ) ) )

  (define (stop-requested?)
    (thread-specific (current-thread) ) )

  (parameterize ((server-hostname hostname)
                 (server-port port))
    (let ((thread (make-thread listen)))
      ;; serve-ready-mutex is used to make sure that the procedure doesn't
      ;; return until the server is ready to accept connections.
      ;; thread-specific value indicates if thread has been told to stop
      (thread-specific-set! thread #f)
      (mutex-lock! server-ready-mutex #f #f)
      (thread-start! thread)
      (mutex-lock! server-ready-mutex #f #f)
      thread) ) ) )


(define (stop-server thread)
  (log-info "stopping server")
  (thread-specific-set! thread #t)
  (thread-join! thread)
  (log-info "server stopped") )



;; Internal Definitions ------------------------------------------------------

;; Read the selector and trim whitespace from the beginning and the end
;; Exceptions, including timeouts, are caught and logged.  If an exception
;; is caught #f is returned
;; Timeout is controlled with tcp-read-timeout
(define (read-selector client-address in)
  (condition-case (string-trim-both (read-line in 255) char-set:whitespace)
    ((exn i/o net timeout)
      (log-warning "client address: ~A, read selector timeout"
                   client-address)
      #f)
    (ex (exn)
      (log-warning "client address: ~A, problem reading selector, ~A"
                   client-address
                   (get-condition-property ex 'exn 'message))
      #f) ) )


;; tcp-accept but handles timeouts governed by parameter tcp-accept-timeout
;; Returns accepted connection ports as values: in out
;; or if timeout, values: #f #f
(define (tcp-accept/handle-timeout listener)
  (condition-case (tcp-accept listener)
    ((exn i/o net timeout) (values #f #f) ) ) )

