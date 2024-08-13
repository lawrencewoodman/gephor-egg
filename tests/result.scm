;;; Tests for the Result definitions

(test-group "result"

  ;; This is important to know for use with test
  (test "Ok can be used with equal?"
        '(#t #f #t #f #t #t #f)
        (let ((values '(("a string" . "a string")
                        ("a string" . "a String")
                        (6 . 6)
                        (6 . 7)
                        (#t . #t)
                        (#f . #f)
                        (#t . #f))))
          (map (lambda (v)
                 (equal? (Ok (car v)) (Ok (cdr v))))
               values) ) )


  ;; This is important to know for use with test
  (test "Error can be used with equal?"
        '(#t #f #t #f #t #t #f)
        (let ((values '(("a string" . "a string")
                        ("a string" . "a String")
                        (6 . 6)
                        (6 . 7)
                        (#t . #t)
                        (#f . #f)
                        (#t . #f))))
          (map (lambda (v)
                 (equal? (Ok (car v)) (Ok (cdr v))))
               values) ) )


  (test "Error raises an exception if a list of strings isn't passed as its value"
        '("bad argument type to variant constructor" "bad argument type to variant constructor")
        (let ((bad-values '("a single string" (5 6))))
          (map (lambda (v)
                 (condition-case (Error v)
                   (ex (exn type) (get-condition-property ex 'exn 'message))))
               bad-values) ) )

  (test "Error accepts a list of strings and they are present when deconstructed"
        '("the first string" "the second string")
        (let ((r (Error '("the first string" "the second string") ) ))
          (cases Result r
                 (Error (e) e)
                 (Ok (v) v) ) ) )

  (test "Ok accepts any value which is present when deconstructed"
        '("a string" 67 '(5 6 7) #t #f)
        (let ((values '("a string" 67 '(5 6 7) #t #f)))
          (map (lambda (r)
                 (cases Result (Ok r)
                        (Ok (v) v)
                        (Error (e) e)))
               values) ) )

  (test "Error? returns #f if value isn't a Result"
        #f
        (Error? "a string") )

  (test "Error? returns #f if value is an Ok Result"
        #f
        (Error? (Ok "a string") ) )

  (test "Error? returns #t if value is an Error Result"
        #t
        (Error? (Error '("a string") ) ) )

  (test "Error-fmt accepts a single string"
        (Error '("a string"))
        (Error-fmt "a string") )

  (test "Error-fmt accepts a format string with arguments"
        (Error '("a string and some numbers as args: 6 7"))
        (Error-fmt "a string and some numbers as args: ~A ~A" 6 7) )

  (test "Error-wrap accepts a single string and places new string in car of error list"
        (Error '("new string" "older string"))
        (Error-wrap (Error '("older string")) "new string") )

  (test "Error-wrap accepts a format string with arguments and places new string in car of error list"
        (Error '("new string with args: 4 5" "older string"))
        (Error-wrap (Error '("older string")) "new string with args: ~A ~A" 4 5) )

  (test "Error-wrap raises an error if Ok used instead of Error as first argument"
        '(error "bad argument type")
        (condition-case (Error-wrap (Ok 5) "a string")
          (ex (exn) (list 'error (get-condition-property ex 'exn 'message) ) ) ) )

  (test "Error-error-ex accepts a single string and places the new string in car of an error list and the message of the exception in cdr of a list"
        (Error '("a string" "this is an exception"))
        (condition-case (error "this is an exception")
          (ex () (Error-ex ex "a string") ) ) )

  (test "Error-error-ex accepts a format string with arguments and places the new string in car of an error list and the message of the exception in cdr of a list"
        (Error '("this is a string with args: 4 5" "this is an exception"))
        (condition-case (error "this is an exception")
          (ex () (Error-ex ex "this is a string with args: 4 5") ) ) )

)

