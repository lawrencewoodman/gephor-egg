;;; Procedures for starting and stopping a Gopher server
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   connection-id start-server stop-server
;;;
;;; Copyright (C) 2024-2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
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
  ;; connection-id-mutex is used to prevent simultaneous changing of connection-id
  ;; number-of-connections is the current number of connections to the server
  ;; connection-id is a unique ID for each connection to make logs messages
  ;;               easier to follow and link together for each connection.
  ;;               This is used to store the value used by the parameter
  ;;               connection-id.
  (let ((server-ready-mutex (make-mutex))
        (connections-mutex (make-mutex))
        (connection-id-mutex (make-mutex))
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

  (define (num-connections)
    (mutex-lock! connections-mutex)
    (let ((count number-of-connections))
      (mutex-unlock! connections-mutex)
      count) )

  (define (wait-for-connections-to-finish)
    (let loop ()
      (mutex-lock! connections-mutex)
      (let ((count number-of-connections))
        (mutex-unlock! connections-mutex)
        (unless (= count 0)
                (loop) ) ) ) )

  ;; Wait until the number of simultaneous connections is less than
  ;; max-connections.
  (define (wait-for-free-connections)
    (let loop ((first-run #t))
      (mutex-lock! connections-mutex)
      (let ((count number-of-connections))
        (mutex-unlock! connections-mutex)
        (when (>= count (max-connections))
              (begin
                (when first-run
                      (log-warning "maximum connections limit reached"
                                   (cons 'number-of-connections
                                         number-of-connections)
                                   (cons 'max-connections
                                         (max-connections))))
                (loop #f) ) ) ) ) )

  (define (next-connection-id)
    (mutex-lock! connection-id-mutex)
    (set! connection-id (add1 connection-id))
    (let ((connection-id connection-id))
      (mutex-unlock! connection-id-mutex)
      connection-id) )

  (define (log-connection-handled)
    (apply log-info
           "connection handled"
           (cons 'num-connections (num-connections))
           (log-context) ) )

  (define (log-exception-in-listen exn)
    (log-error "exception in listen"
                 (cons 'exception-msg
                       (get-condition-property exn 'exn 'message) ) ) )

  ;; TODO: maybe shouldn't mention exception in this
  (define (log-exception-in-handle-thread in exn)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (log-error "exception in handler thread"
                 (cons 'exception-msg (get-condition-property exn 'exn 'message))
                 (cons 'client-address client-address) ) ) )

  ;; TODO: probably should mention exception in this
  (define (log-exception-in-run-handler exn)
    (apply log-error "exception raised in run handler"
                     (cons 'exception-msg
                           (get-condition-property exn 'exn 'message))
                     (cons 'num-connections
                           (num-connections))
                     (log-context) ) )

  (define (run-handler handler request out)
    (handle-exceptions exn
                       (begin
                         (log-exception-in-run-handler exn)
                         (send-response/error-menu "resource unavailable" out))
                       (let ((response (handler request)))
                         (if response
                             (when (send-response response out)
                                   (log-connection-handled))
                             (begin
                               ;; TODO: is this still appropriate?
                               (apply log-warning "error in handler"
                                      (cons 'num-connections (num-connections))
                                      (log-context))
                               (send-response/error-menu "resource unavailable"
                                                          out) ) ) ) ) )

  (define (handle-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (let ((selector (read-selector client-address in)))
        (when selector
              (parameterize ((log-context (list (cons 'connection-id (next-connection-id))
                                                (cons 'client-address client-address)
                                                (cons 'selector selector))))
                (let ((handler (router-match router selector))
                      (request (make-request selector client-address)))
                  (if handler
                      (run-handler handler request out)
                      (begin
                        (apply log-warning "no handler for selector"
                               (cons 'num-connections (num-connections))
                               (log-context))
                        (send-response/error-menu "path not found" out))))))
        (close-input-port in)
        (close-output-port out) ) ) )


  (define (start-connect-thread in out)
    (thread-start!
      (make-thread
        (lambda ()
          (inc-connection-count)
          ; We need an exception handler here and in listen so that
          ; the connection-count gets decreased if an exception is raised
          (handle-exceptions exn
                             (log-exception-in-handle-thread in exn)
                             (handle-connect in out))
          (dec-connection-count)))))

  ;; Continuously listens to connections to the port and arranges
  ;; for the connections to be handled.  This is designed to run as a
  ;; thread which is stopped by stop-sever.
  ;; If max connections reached then it will stop accepting connections
  ;; until the number of simultaneous connections drops.
  (define (listen)
    (handle-exceptions exn
                       (log-exception-in-listen exn)
      (parameterize ((tcp-accept-timeout (or (tcp-accept-timeout) 5000)))
        (let ((listener (tcp-listen port)))
          (mutex-unlock! server-ready-mutex)
          (let loop ()
            (wait-for-free-connections)
            (let-values (((in out) (tcp-accept/handle-timeout listener)))
              (when (and in out)
                    (start-connect-thread in out)))
              (unless (stop-requested?)
                      (loop)))
          (wait-for-connections-to-finish)
          (tcp-close listener) ) ) ) )

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
;; TODO: Think about what tcp-read-timeout should be because this could
;; TODO: make a DOS attack easier the longer the timeout
(define (read-selector client-address in)
  (condition-case (string-trim-both (read-line in 255) char-set:whitespace)
    ((exn i/o net timeout)
      (apply log-warning
             "read selector timeout"
             (cons 'client-address client-address)
             (log-context))
      #f)
    (ex (exn)
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

