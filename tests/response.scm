;;; Tests for the response sending procedures


(test-group "response"

  (test "send-response outputs a response to the given output port"
        "this is the response"
        (let ((out (open-output-string)))
          (send-response "this is the response" out)
          (let ((response (get-output-string out)))
            (close-output-port out)
            response) ) )


  (test "send-response raises an error if the response > max-response-size"
        '((() (send-response "response is too big to send")) "123456")
        (let ((test-responses '("123456" "1234567"))
              (out (open-output-string)))
          (parameterize ((max-response-size 6))
            (list (map (lambda (r)
                         (handle-exceptions ex
                           (list (get-condition-property ex 'exn 'location)
                                 (get-condition-property ex 'exn 'message))
                           (send-response r out)
                           '()))
                       test-responses)
                  (let ((output (get-output-string out)))
                    (close-output-port out)
                    output) ) ) ) )


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
