;;; Tests for the router procedures

(test-group "router"
  (test "make-router makes an empty router if no arguments passed to it"
        '()
        (make-router) )


  (test "make-router makes a router with the routes added and sorted"
        '("" "hi" "hrl" "hrl*" "hi*" "hn*" "hr*" "*")
        (let ((router (make-router (cons "" dummy)
                                   (cons "*" dummy)
                                   (cons "hi" dummy)
                                   (cons "hi*" dummy)
                                   (cons "hr*" dummy)
                                   (cons "hn*" dummy)
                                   (cons "hrl*" dummy)
                                   (cons "hrl" dummy))))
          (map car router) ) )


  (test "make-router raises an exception if there is a duplicate pattern"
        '(make-router "duplicate pattern: *")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (make-router (cons "" dummy)
                       (cons "*" dummy)
                       (cons "*" dummy) ) ) )


  (test "make-router raises an exception if there are multiple '*' in pattern"
        '(make-router "invalid pattern: h*h*")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (make-router (cons "" dummy)
                       (cons "h*h*" dummy) ) ) )


  (test "make-router raises an exception if there is a '*' in pattern but not at the end"
        '(make-router "invalid pattern: h*h")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (make-router (cons "" dummy)
                       (cons "h*h" dummy) ) ) )


  (test "router-add adds a route and sorts properly"
        '("" "hi" "hrl" "hrl*" "hi*" "hn*" "hr*" "*")
        (let* ((router (make-router))
               (router (router-add router "" dummy))
               (router (router-add router "*" dummy))
               (router (router-add router "hi" dummy))
               (router (router-add router "hi*" dummy))
               (router (router-add router "hr*" dummy))
               (router (router-add router "hn*" dummy))
               (router (router-add router "hrl*" dummy))
               (router (router-add router "hrl" dummy)))
          (map car router) ) )


  (test "router-add raises an exception if a pattern is added that already exists in router"
        '(router-add "pattern already exists in router: *")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (let* ((router (make-router))
                 (router (router-add router "" dummy))
                 (router (router-add router "*" dummy))
                 (router (router-add router "*" dummy) ) )
            (map car router) ) ) )


  (test "router-add raises an exception if there are multiple '*' in pattern"
        '(router-add "invalid pattern: h*h*")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
        (let ((router (make-router)))
          (router-add router "h*h*" dummy) ) ) )

  (test "router-add raises an exception if there is a  '*' in pattern but not at the end"
        '(router-add "invalid pattern: h*h")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
        (let ((router (make-router)))
          (router-add router "h*h" dummy) ) ) )


  (test "router-match finds the correct handler for a selector"
        '("pat:*" "pat:" "pat:hi" "pat:hi*" "pat:hr*" "pat:hr*" "pat:hrl" "pat:hrl*")
        (let* ((router (make-router (cons "" (lambda () "pat:"))
                                    (cons "*" (lambda () "pat:*"))
                                    (cons "hi" (lambda () "pat:hi"))
                                    (cons "hi*" (lambda () "pat:hi*"))
                                    (cons "hr*" (lambda () "pat:hr*"))
                                    (cons "hn*" (lambda () "pat:hn*"))
                                    (cons "hrl*" (lambda () "pat:hrl*"))
                                    (cons "hrl" (lambda () "pat:hrl"))))
               (selectors '("/" "" "hi" "hil" "hr" "hrn" "hrl" "hrln")))
          (map (lambda (selector)
                 (let ((handler (router-match router selector)))
                       (handler)))
               selectors) ) )

  (test "router-match returns #f if route can't be found"
        #f
        (let* ((router (make-router (cons "" (lambda () "pat:"))
                                    (cons "hi" (lambda () "pat:hi")))))
          (router-match router "fred") ) )

)

