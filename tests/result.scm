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


  (test "Ok accepts any value which is present when deconstructed"
        '("a string" 67 '(5 6 7) #t #f)
        (let ((values '("a string" 67 '(5 6 7) #t #f)))
          (map (lambda (r)
                 (cases Result (Ok r)
                        (Ok (v) v)
                        (Error (e) #f)))
               values) ) )


  ;; This is important to know for use with test
  (test "Not-Applicable can be used with equal?"
        '(#t #t #f #f)
        (let ((values '((#t . #t)
                        (#f . #f)
                        (#t . #f)
                        (#f . #t))))
          (map (lambda (v)
                 (equal? (Not-Applicable (car v)) (Not-Applicable (cdr v))))
               values) ) )


  ;; This is important to know for use with test
  (test "Error can be used with equal?"
        '(#t #f #f #f #f #f)
        (let ((values '((("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f))))

                         (("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("b string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f))))

                        (("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("a string"
                          ((file . "/tmp/tr") (id . 4) (in . #t) (out . #f))))

                        (("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("a string"
                          ((file . "/tmp/t") (id . 5) (in . #t) (out . #f))))
                    
                        (("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("a string"
                          ((file . "/tmp/t") (id . 4) (in . #f) (out . #f))))

                        (("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #f)))
                         .
                         ("a string"
                          ((file . "/tmp/t") (id . 4) (in . #t) (out . #t)))))))
          (map (lambda (v)
                 (let ((x (car v))  (y (cdr v)))
                   (equal? (Error (first x) (second x))
                           (Error (first y) (second y)))))
               values) ) )


  (test "Error raises an exception if the msg isn't string"
        '("bad argument type to variant constructor" "bad argument type to variant constructor" "bad argument type to variant constructor")
        (let ((bad-values '(#t (5 6) 4)))
          (map (lambda (v)
                 (condition-case (Error v '((file . "/tmp/t")))
                   (ex (exn type) (get-condition-property ex 'exn 'message))))
               bad-values) ) )


  (test "Error accepts a string as the msg and it is present when deconstructed"
        "a string"
        (cases Result (Error "a string" '((file ."/tmp/t")))
               (Error (msg log-entries) msg)
               (Ok (v) #f)))


  (test "Error accepts a list of pairs as the log-entries and they are present when deconstructed"
        '((file . "/tmp/t") (id . 3))
        (cases Result (Error "a string" '((file ."/tmp/t") (id . 3)))
               (Error (msg log-entries) log-entries)
               (Ok (v) #f)))


)

