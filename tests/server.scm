;;; Tests for the main server procedures

(test-group "server"

  (define (gopher-test-get port selector)
    (let-values (((in out) (tcp-connect "localhost" port)))
      (write-line selector out)
      (let ((response (read-string #f in)))
        (close-input-port in)
        (close-output-port out)
        response) ) )

  (define (start-test-server port router)
    ;; tcp-accept-timeout is set to make checking if due to stop quicker
    (parameterize ((tcp-accept-timeout 1))
      (start-server hostname: "localhost" port: port router: router) ) )


  (test "server responds correctly to simple routes without a splat"
        '("hello friend" "bye friend")
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (request)
                                                    "hello friend"))
                                    (cons "bye" (lambda (request)
                                                  "bye friend"))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                '("hello" "bye"))))
            (stop-server thread)
            responses) ) )


  (test "server returns an 'path not found' error menu if a route doesn't exist for the selector"
        "3path not found\t\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (request)
                                                    "hello friend"))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "bye")))
            (stop-server thread)
            response) ) )


  (test "server returns a 'resource unavailable"
        "3resource unavailable\t\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (request)
                                                    (error "this is an error")))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "hello")))
            (stop-server thread)
            response) ) )


  (test "server returns a 'resource too big to send' error menu if data to send is > max-file-size bytes"
        "3resource is too big to send\t\tlocalhost\t7070\r\n.\r\n"
        (let ((port 7070)
              (router (make-router (cons "hello" (lambda (request)
                                                         (string-intersperse
                                                           '("1234567890"
                                                             "1234567890"
                                                             "1234567890"
                                                             "1234567890"
                                                             "1234567890"
                                                             "1234567890"
                                                             "1234567890"
                                                             "1234567890")))))))
          (parameterize ((max-file-size 60))
            (let* ((thread (start-test-server port router))
                   (response (gopher-test-get port "hello")))
              (stop-server thread)
              response) ) ) )


  (test "server removes whitespace at beginning and end of selectors before passing to handlers"
        '("test" "test" "test")
        (let* ((port 7070)
               (router (make-router (cons "*" (lambda (request)
                                                (request-selector request)))))
               (thread (start-test-server port router))
               (selectors '("  test" "test  " "  test  ")))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                selectors)))
            (stop-server thread)
            responses) ) )


  (test "server leaves leading and terminating '/' characters in selectors intact"
        '("/test" "test/" "/test/" "/  test" "test  /")
        (let* ((port 7070)
               (router (make-router (cons "*" (lambda (request)
                                                (request-selector request)))))
               (thread (start-test-server port router))
               (selectors '("/test" "test/" "/test/" "/  test" "test  /")))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                selectors)))
            (stop-server thread)
            responses) ) )


  (test "server logs a warning message if there is a timeout while waiting for the selector"
        "[WARNING] client address: 127.0.0.1, read selector timeout\n"
        (let ((log-port (open-output-string)))
          (parameterize ((tcp-read-timeout 0)
                         (log-level 30)
                         (warning-logger-config
                           (config-logger (warning-logger-config)
                                          port: log-port)))
            (let* ((port 7070)
                   (router (make-router (cons "*" (lambda (request)
                                                    (request-selector request)))))
                   (thread (start-test-server port router)))
              (let-values (((in out) (tcp-connect "localhost" port)))
                ;; TODO: Add a server-ready? function so that we don't need to sleep
                (sleep 1)
                (close-input-port in)
                (close-output-port out)
                (stop-server thread)
                (get-output-string log-port) ) ) ) ) )


  (test "server logs a warning message if the connection is broken while waiting for the selector"
        "[WARNING] client address: 127.0.0.1, problem reading selector, bad argument type\n"
        (let ((log-port (open-output-string)))
          (parameterize ((tcp-read-timeout 2)
                         (log-level 30)
                         (warning-logger-config
                           (config-logger (warning-logger-config)
                                          port: log-port)))
            (let* ((port 7070)
                   (router (make-router (cons "*" (lambda (request)
                                                    (request-selector request)))))
                   (thread (start-test-server port router)))
              (let-values (((in out) (tcp-connect "localhost" port)))
                (close-input-port in)
                (close-output-port out)
                (stop-server thread)
                (get-output-string log-port) ) ) ) ) )

)

