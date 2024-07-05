(import scheme
        test
        (chicken base)
        (chicken condition)
        (chicken format)
        (chicken file)
        (chicken file posix)
        (chicken load)
        (chicken pathname)
        (chicken process-context)
        (chicken port)
        (chicken string))


(load-relative "../server.scm")
(import gophser)

(define dummy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Router Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Menu Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-group "menu"

  (test "make-render adds correct .<cr><lf> to end of menu"
        (string-intersperse '(
          "1Somewhere interesting\t/interesting\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((menu (list (menu-item "menu" "Somewhere interesting" "/interesting" "localhost" 70))))
          (menu-render menu)))

  (test "make-item-info wraps info text"
        (string-intersperse '(
          "iThis is some interesting text that you might like to read about if you have the\tFAKE\tlocalhost\t70"
          "itime to look at it but the main point is that it is there to see if the text\tFAKE\tlocalhost\t70"
          "iwrapping works properly\tFAKE\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((text (string-intersperse '(
                      "This is some interesting text that you might like to read about if you have the time to look at it but the main"
                      "point is that it is there to see"
                      "if the text wrapping works properly")
                      "\n"))
               ;; TODO: what should the selector, hostname and port be?
               (menu (menu-item-info text "FAKE" "localhost" 70)))
          (menu-render menu)))

  ;; TODO: test menu-item raises error on Info or i

  ;; TODO: Will need to add more types as they become supported
  ;; TODO: Should we accept any single letters and maybe just warn
  ;; TODO: if they are unknown?
  (test "make-item handles a range of types and their human readable names"
        (string-intersperse '(
          "0Some text\ttext text\tlocalhost\t70"
          "0Some text\ttext 0\tlocalhost\t70"
          "1A menu\tmenu menu\tlocalhost\t70"
          "1A menu\tmenu 1\tlocalhost\t70"
          "hSome html\thtml html\tlocalhost\t70"
          "hSome html\thtml h\tlocalhost\t70"
          "ISome image\timage image\tlocalhost\t70"
          "ISome image\timage I\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((menu '())
               (menu (cons (menu-item "text" "Some text" "text text" "localhost" 70) menu))
               (menu (cons (menu-item "0" "Some text" "text 0" "localhost" 70) menu))
               (menu (cons (menu-item "menu" "A menu" "menu menu" "localhost" 70) menu))
               (menu (cons (menu-item "1" "A menu" "menu 1" "localhost" 70) menu))
               (menu (cons (menu-item "html" "Some html" "html html" "localhost" 70) menu))
               (menu (cons (menu-item "h" "Some html" "html h" "localhost" 70) menu))
               (menu (cons (menu-item "image" "Some image" "image image" "localhost" 70) menu))
               (menu (cons (menu-item "I" "Some image" "image I" "localhost" 70) menu)))
          (menu-render (reverse menu))))

)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-group "handlers"

  ;; The path of the fixtures directory
  (define fixtures-dir
    (let loop ((dirs (list (current-directory) (make-pathname (current-directory) "tests"))))
      (if (null? dirs) (error "can't find fixtures directory"))
      (let ((try-path (make-pathname (car dirs) "fixtures")))
        (if (and (file-exists? try-path) (directory? try-path))
            try-path
            (loop (cdr dirs))))))

  (test "serve-path supportes empty selector and empty prefix"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "1dir-a\tdir-a\tlocalhost\t70"
          "1dir-b\tdir-b\tlocalhost\t70"
          "0a.txt\ta.txt\tlocalhost\t70"
          "0b.txt\tb.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((output-port (open-output-string))
               (context (make-context "localhost" 70))
               (request (make-request "" output-port "127.0.0.1"))
               (selector-prefix ""))
          (serve-path context request fixtures-dir selector-prefix) ) )

  ;; TODO: What should happen if selector is '/' and prefix is "" ?
  (test "serve-path supportes '/' selector and '/' prefix"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "1dir-a\t/dir-a\tlocalhost\t70"
          "1dir-b\t/dir-b\tlocalhost\t70"
          "0a.txt\t/a.txt\tlocalhost\t70"
          "0b.txt\t/b.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((output-port (open-output-string))
               (context (make-context "localhost" 70))
               (request (make-request "/" output-port "127.0.0.1"))
               (selector-prefix "/"))
          (serve-path context request fixtures-dir selector-prefix) ) )

  (test "serve-path supportes subpath ('/dir-a') selector and '/' prefix"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "0aa.txt\t/dir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\t/dir-a/ab.txt\tlocalhost\t70"
          ;; TODO: Incorrect item type of ac.bin
          "0ac.bin\t/dir-a/ac.bin\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((output-port (open-output-string))
               (context (make-context "localhost" 70))
               (request (make-request "/dir-a" output-port "127.0.0.1"))
               (selector-prefix "/"))
          (serve-path context request fixtures-dir selector-prefix) ) )

  ;; TODO: Decide if this is the best way of handling an error
  (test "serve-path raises an exception if selector subpath contains '..'"
        "invalid selector"
        (handle-exceptions ex
          (get-condition-property ex 'exn 'message)
          ;; Directories come before regular files and each in alphabetical order
          (let* ((output-port (open-output-string))
                 (context (make-context "localhost" 70))
                 (request (make-request "../dir-a" output-port "127.0.0.1"))
                 (selector-prefix ""))
            (serve-path context request fixtures-dir selector-prefix) ) ) )

  ;; TODO: Decide if this is the best way of handling an error
  (test "serve-path raises an exception if selector subpath contains './'"
        "invalid selector"
        (handle-exceptions ex
          (get-condition-property ex 'exn 'message)
          ;; Directories come before regular files and each in alphabetical order
          (let* ((output-port (open-output-string))
                 (context (make-context "localhost" 70))
                 (request (make-request "./dir-a" output-port "127.0.0.1"))
                 (selector-prefix ""))
            (serve-path context request fixtures-dir selector-prefix) ) ) )

  ;; TODO: Decide if this is the best way of handling an error
  (test "serve-path raises an exception if selector sub path contains a '\\'"
        "invalid selector"
        (handle-exceptions ex
          (get-condition-property ex 'exn 'message)
          ;; Directories come before regular files and each in alphabetical order
          (let* ((output-port (open-output-string))
                 (context (make-context "localhost" 70))
                 (request (make-request "dir-a\\fred" output-port "127.0.0.1"))
                 (selector-prefix ""))
            (serve-path context request fixtures-dir selector-prefix) ) ) )


  ;; TODO: Decide if this is the best way of handling an error
  (test "serve-path raises an exception if local-dir ends with a '/'"
        "invalid local-dir"
        (handle-exceptions ex
          (get-condition-property ex 'exn 'message)
          ;; Directories come before regular files and each in alphabetical order
          (let* ((output-port (open-output-string))
                 (context (make-context "localhost" 70))
                 (request (make-request "/dir-a" output-port "127.0.0.1"))
                 (selector-prefix "")
                 (local-dir (sprintf "~A/" fixtures-dir)))
            (serve-path context request local-dir selector-prefix) ) ) )


  ;; TODO: Decide if this is the best way of handling an error
  (test "serve-path returns the contents of a binary file"
        "This is text followed by a null (00)\x00 now some more text."
        ;; Directories come before regular files and each in alphabetical order
        (let* ((output-port (open-output-string))
               (context (make-context "localhost" 70))
               (request (make-request "/dir-a/ac.bin" output-port "127.0.0.1"))
               (selector-prefix ""))
          (serve-path context request fixtures-dir selector-prefix) ) )


)

