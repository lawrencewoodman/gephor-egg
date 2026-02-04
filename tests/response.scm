;;; Tests for the response sending procedures


(test-group "response"

  (test "send-response outputs a response to the given output port and returns #t"
        '(#t "this is the response")
        (let ((out (open-output-string)))
          (list (send-response "this is the response" out)
                (let ((response (get-output-string out)))
                  (close-output-port out)
                  response) ) ) )


  (test "send-response outputs an error menu to the given output port and returns #f if the response > max-response-size"
        '((#t #f) "1234563resource unavailable\t\tlocalhost\t70\r\n.\r\n")
        (let ((test-responses '("123456" "1234567"))
              (out (open-output-string)))
          (parameterize ((max-response-size 6))
            (list (map (lambda (r) (send-response r out))
                       test-responses)
                  (let ((output (get-output-string out)))
                    (close-output-port out)
                    output) ) ) ) )


  (test "send-response logs an error message if the response > max-response-size"
        "ts=#t level=error msg=\"response is too big to send\" connection-id=3\n"
        (let ((out (open-output-string))
              (log-test-port (open-output-string)))
          (parameterize ((max-response-size 6)
                         (log-level 'warning)
                         (log-port log-test-port)
                         (log-context (list (cons 'connection-id 3))))
            (send-response "1234567" out))
            (confirm-log-entries-valid-timestamp (get-output-string log-test-port) ) ) )


  (test "send-response/error-menu outputs a menu with msg as the username"
        "3this is a message\t\tlocalhost\t70\r\n.\r\n"
        (let ((out (open-output-string)))
          (send-response/error-menu "this is a message" out)
          (let ((response (get-output-string out)))
            (close-output-port out)
            response) ) )


  (test "send-response/error-menu truncates the error msg at 160 characters"
        (list (sprintf "3~A\t\tlocalhost\t70\r\n.\r\n"
                       (make-string 160 #\A))
              (+ 160 20))
        (let ((out (open-output-string)))
          (send-response/error-menu (make-string 600 #\A) out)
          (let ((response (get-output-string out)))
            (close-output-port out)
            (list response (string-length response) ) ) ) )


)
