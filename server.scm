;;; Procedures for starting and stopping a Gopher server
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   connection-id start-server stop-server
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
  ;; connection-id is a unique ID for each connection to make logs messages
  ;;               easier to follow and link together for each connection.
  ;;               This is used to store the value used by the parameter
  ;;               connection-id.
  (let ((server-ready-mutex (make-mutex))
        (connections-mutex (make-mutex))
        (max-number-of-connections 50)   ;; TODO: parameterize this?
        (number-of-connections 0)
        (connection-id 0))

  (define (inc-connection-count)
    (mutex-lock! connections-mutex)
    (set! number-of-connections (add1 number-of-connections))
    (mutex-unlock! connections-mutex) )

  (define (dec-connection-count)
    (mutex-lock! connections-mutex)
    (set! number-of-connections (sub1 number-of-connections))
    (mutex-unlock! connections-mutex) )

  (define (max-connections-reached?)
    (mutex-lock! connections-mutex)
    (let ((count number-of-connections))
      (mutex-unlock! connections-mutex)
      (>= count max-number-of-connections) ) )

  (define (num-connections)
    (mutex-lock! connections-mutex)
    (let ((count number-of-connections))
      (mutex-unlock! connections-mutex)
      count) )

  ;; TODO: Give this it's own mutex
  (define (next-connection-id)
    (mutex-lock! connections-mutex)
    (set! connection-id (add1 connection-id))
    (let ((connection-id connection-id))
      (mutex-unlock! connections-mutex)
      connection-id) )


  ;; Parameter: max-file-size controls the maximum size file
  ;; that can be written
  (define (handle-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (let ((selector (read-selector client-address in)))
        ;; TODO: What happens if selector is #f?
        (when selector
              (parameterize ((log-context (list (cons 'connection-id (next-connection-id))
                                                (cons 'client-address client-address)
                                                (cons 'selector selector))))
                (let* ((handler (router-match router selector))
                       (request (make-request selector client-address))
                       (response
                         (and handler
                             (condition-case (handler request)
                               (ex ()
                                 ;; TODO: Should this log-error list the handler
                                 (apply log-error "exception raised by handler"
                                        (cons 'exception-msg (get-condition-property ex 'exn 'message))
                                        (cons 'num-connections (num-connections))
                                        (log-context))
                                 (make-rendered-error-menu request "resource unavailable"))))))
                  (if response
                      (if (> (string-length response) (max-file-size))
                          (begin
                            (apply log-error "data is too big to send"
                                   (cons 'num-connections (num-connections))
                                   (log-context))
                            (write-string (make-rendered-error-menu request "resource is too big to send")
                                          (max-file-size)
                                          out))
                          (begin
                            (write-string response (max-file-size) out)
                            (when handler
                              (apply log-info "connection handled"
                                     (cons 'num-connections (num-connections))
                                     (log-context)))))
                      (begin
                        (apply log-warning "no handler for selector"
                               (cons 'num-connections (num-connections))
                               (log-context))
                        (write-string (make-rendered-error-menu request "path not found")
                                      (max-file-size)
                                      out))))))
        (close-input-port in)
        (close-output-port out ) ) ) )

  (define (start-connect-thread in out)
    (thread-start!
      (make-thread
        (lambda ()
          (inc-connection-count)
          (handle-exceptions ex
            ;; TODO: Improve error message
            ;; TODO: Add connection-id - would it always be updated?`
            (log-error "exception in handler thread"
                       (cons 'exception-msg (get-condition-property ex 'exn 'message)))
            (handle-connect in out))
          (dec-connection-count)))))

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
          (if (max-connections-reached?)
              (log-warning "maximum connections limit reached"
                           (cons 'number-of-connections number-of-connections))
              (let-values (((in out) (tcp-accept/handle-timeout listener)))
                (when (and in out)
                      (start-connect-thread in out))))
            (unless (stop-requested?)
                    (loop)))
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
      (log-info "server started" (cons 'hostname hostname) (cons 'port port))
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
      ;; TODO: Add log-context ?
      (log-warning "read selector timeout"
                   (cons 'client-address client-address))
      #f)
    (ex (exn)
      ;; TODO: Should this be a warning or error log level?
      ;; TODO: Add log-context ?
      (log-warning "exception when reading selector"
                   (cons 'client-address client-address)
                   (cons 'exception-msg (get-condition-property ex 'exn 'message)))
      #f) ) )


;; tcp-accept but handles timeouts governed by parameter tcp-accept-timeout
;; Returns accepted connection ports as values: in out
;; or if timeout, values: #f #f
(define (tcp-accept/handle-timeout listener)
  (condition-case (tcp-accept listener)
    ((exn i/o net timeout) (values #f #f) ) ) )

