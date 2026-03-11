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


  (test "server responds correctly to routes without a splat"
        '("hello friend" "bye friend")
        (let* ((port 7070)
               (router (make-router (cons "hello"
                                          (lambda (request) "hello friend"))
                                    (cons "bye"
                                          (lambda (request) "bye friend"))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                '("hello" "bye"))))
            (stop-server thread)
            responses) ) )


  (test "server responds correctly to routes with a splat"
        '("blog selector: blog/something/today/scheme"
          "guestbook selector: guestbook/sign/hello")
        (let* ((port 7070)
               (router (make-router (cons "blog/something/*"
                                          (lambda (request)
                                            (sprintf "blog selector: ~A"
                                                     (request-selector request))))
                                    (cons "guestbook/sign/*"
                                          (lambda (request)
                                            (sprintf "guestbook selector: ~A"
                                                     (request-selector request))))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                '("blog/something/today/scheme"
                                  "guestbook/sign/hello"))))
            (stop-server thread)
            responses) ) )


  (test "server returns an 'path not found' error menu if a route doesn't exist for the selector"
        "3path not found\t\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello"
                                          (lambda (request) "hello friend"))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "bye")))
            (stop-server thread)
            response) ) )


  (test "server returns a 'resource unavailable' error menu and logs an error if an exception is raised"
        '("3resource unavailable\t\tlocalhost\t7070\r\n.\r\n"
          "ts=#t level=error msg=\"exception raised in run handler\" exception-location=#f exception-msg=\"this is an error\" num-connections=1 client-address=127.0.0.1 selector=hello\n")
        (let ((port 7070)
              (router (make-router (cons "hello" (lambda (request)
                                                    (error "this is an error"))))))
          (run/get-log 'error
                       (let* ((thread (start-test-server port router))
                              (response (gopher-test-get port "hello")))
                         (stop-server thread)
                         response) ) ) )


  (test "server returns a 'resource unavailable' error menu and logs an error if data to send is > max-response-size bytes"
        '("3resource unavailable\t\tlocalhost\t7070\r\n.\r\n"
          "ts=#t level=error msg=\"exception raised in run handler\" exception-location=send-response exception-msg=\"response is too big to send\" num-connections=1 client-address=127.0.0.1 selector=hello\n")
        (let ((port 7070)
              (router (make-router (cons "hello"
                                         (lambda (request)
                                           (make-string 61 #\A))))))
          (parameterize ((max-response-size 60))
            (run/get-log 'error
                         (let* ((thread (start-test-server port router))
                                (response (gopher-test-get port "hello")))
                           (stop-server thread)
                           response) ) ) ) )


  (test "server removes whitespace at beginning and end of selectors before passing to handlers"
        '("test" "test" "test")
        (let* ((port 7070)
               (router (make-router (cons "*"
                                          (lambda (request)
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
               (router (make-router (cons "*"
                                          (lambda (request)
                                            (request-selector request)))))
               (thread (start-test-server port router))
               (selectors '("/test" "test/" "/test/" "/  test" "test  /")))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                selectors)))
            (stop-server thread)
            responses) ) )


  (test "server responds correctly to request going to serve-file"
        '("hello\n" "hello aa\n")
        (let* ((port 7070)
               (router (make-router (cons "*"
                                          (lambda (request)
                                            (serve-file fixtures-dir request)))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector) (gopher-test-get port selector))
                                '("a.txt" "dir-a/aa.txt"))))
            (stop-server thread)
            responses) ) )


  (test "server logs a warning message if there is a timeout while waiting for the selector"
        "ts=#t level=warning msg=\"read selector timeout\" client-address=127.0.0.1\n"
        (parameterize ((tcp-read-timeout 0)
                       (log-level 'warning)
                       (log-port (open-output-string)))
          (let* ((port 7070)
                 (router (make-router (cons "*"
                                            (lambda (request)
                                              (request-selector request)))))
                 (thread (start-test-server port router))
                 (test-timeout-limit (+ (current-process-milliseconds) 1000)))
            (let-values (((in out) (tcp-connect "localhost" port)))
              (let ((final-log-entry
                      (let loop ()
                        (if (> (current-process-milliseconds) test-timeout-limit)
                            "test timeout"
                             (let ((log-entry (get-output-string (log-port))))
                               (if (not (equal? log-entry ""))
                                   log-entry
                                   (loop)))))))
                   (close-input-port in)
                   (close-output-port out)
                   (stop-server thread)
                   (confirm-log-entries-valid-timestamp final-log-entry) ) ) ) ) )


  (test "server logs a warning message if the connection is broken while waiting for the selector"
        "ts=#t level=warning msg=\"read selector failed, connection probably terminated\" client-address=127.0.0.1\n"
        (let* ((port 7070)
               (router (make-router (cons "*"
                                          (lambda (request)
                                            (request-selector request))))))
          (parameterize ((tcp-read-timeout 2))
            (second (run/get-log 'warning
                      (let ((thread (start-test-server port router)))
                        (let-values (((in out) (tcp-connect "localhost" port)))
                          (close-input-port in)
                          (close-output-port out)
                          (stop-server thread)) ) ) ) ) ) )


  (test "start-server logs an info message to say the server has started"
        "ts=#t level=info msg=\"server started\" hostname=localhost port=7070"
        (let* ((port 7070)
               (router (make-router (cons "*"
                                          (lambda (request)
                                            (request-selector request))))))
          (car (string-split
            (second (run/get-log 'info
              (let ((thread (start-server hostname: "localhost"
                                          port: port
                                          router: router)))
                (let-values (((in out) (tcp-connect "localhost" port)))
                  (close-input-port in)
                  (close-output-port out)
                  (stop-server thread)))))
            "\n" #t) ) ) )



)

