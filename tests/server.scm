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
      (start-server "localhost" port router) ) )


  (test "server responds correctly to simple routes without a splat"
        '("hello friend" "bye friend")
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (context request)
                                                    "hello friend"))
                                    (cons "bye" (lambda (context request)
                                                  "bye friend"))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                '("hello" "bye"))))
            (stop-server thread)
            responses) ) )


  (test "server returns an 'unknown path' error menu if a route doesn't exist for the selector"
        "3path not found\tbye\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (context request)
                                                    "hello friend"))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "bye")))
            (stop-server thread)
            response) ) )


  (test "server returns a 'server error' error menu if a handler raises an exception"
        "3server error\thello\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (context request)
                                                    (error "this is an error")))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "hello")))
            (stop-server thread)
            response) ) )

)

