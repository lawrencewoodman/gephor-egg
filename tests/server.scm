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
                                                    (Ok "hello friend")))
                                    (cons "bye" (lambda (request)
                                                  (Ok "bye friend")))))
               (thread (start-test-server port router)))
          (let ((responses (map (lambda (selector)
                                  (gopher-test-get port selector))
                                '("hello" "bye"))))
            (stop-server thread)
            responses) ) )


  (test "server returns an 'unknown path' error menu if a route doesn't exist for the selector"
        "3path not found\t\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (request)
                                                    (Ok "hello friend")))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "bye")))
            (stop-server thread)
            response) ) )


  (test "server returns a 'resource can not be accessed' error menu if a handler raises an exception"
        "3resource can not be accessed\t\tlocalhost\t7070\r\n.\r\n"
        (let* ((port 7070)
               (router (make-router (cons "hello" (lambda (request)
                                                    (error "this is an error")))))
               (thread (start-test-server port router)))
          (let ((response (gopher-test-get port "hello")))
            (stop-server thread)
            response) ) )


  (test "server truncates data sent to max-file-size bytes"
        "hello frien"
        (let ((port 7070)
              (router (make-router (cons "hello" (lambda (request)
                                                         (Ok "hello friend"))))))
          (parameterize ((max-file-size 11))
            (let* ((thread (start-test-server port router))
                   (response (gopher-test-get port "hello")))
              (stop-server thread)
              response) ) ) )


  (test "server removes whitespace and '/' characters at beginning and end of selectors before passing to handlers"
        '(#t #t #t #t #t #t #t #t)
        (let* ((port 7070)
               (router (make-router (cons "*" (lambda (request)
                                                (Ok (request-selector request))))))
               (thread (start-test-server port router))
               (selectors '("/test" " /test" " / test" "/ test" " test"
                            "test/" "test//" "test / /")))
          (let ((responses (map (lambda (selector)
                                  (string=? "test" (gopher-test-get port selector)))
                                selectors)))
            (stop-server thread)
            responses) ) )

)

