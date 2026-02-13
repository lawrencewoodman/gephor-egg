;;; Tests for the misc procedures


(test-group "misc"

  (test "selector->local-path returns false if selector contains '..'"
        (list 'selector->local-path
              (sprintf "path isn't safe: ~A"
                       (make-pathname fixtures-dir "../dir-a") ) )
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path fixtures-dir "../dir-a") ) )

  (test "selector->local-path raises an error if selector contains './'"
        (list 'selector->local-path
              (sprintf "path isn't safe: ~A"
                       (make-pathname fixtures-dir "./dir-a") ) )
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path fixtures-dir "./dir-a") ) )

  (test "selector->local-path raises an error if selector contains a '\\'"
        (list 'selector->local-path
              (sprintf "path isn't safe: ~A"
                       (make-pathname fixtures-dir "dir-a\\fred") ) )
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path fixtures-dir "dir-a\\fred") ) )

  (test "selector->local-path raises an error if root-dir contains ./"
        '(selector->local-path "path isn't safe: /tmp/./this/dir-a")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path "/tmp/./this" "dir-a") ) )

  (test "selector->local-path raises an eror if root-dir contains .."
        '(selector->local-path "path isn't safe: /../dir-a")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path "/.." "dir-a") ) )

  (test "selector->local-path raises an error if root-dir contains \\"
        '(selector->local-path "path isn't safe: /\\/dir-a")
        (handle-exceptions ex
          (list (get-condition-property ex 'exn 'location)
                (get-condition-property ex 'exn 'message))
          (selector->local-path "/\\" "dir-a") ) )

  (test "selector->local-path doesn't allow percent decoding to turn %2E%2E into .."
        (make-pathname fixtures-dir "dir-a/%2E%2E")
        (selector->local-path fixtures-dir "dir-a/%2E%2E") )

  (test "selector->local-path forms a local-path from root-dir and selector"
        (make-pathname fixtures-dir "dir-a")
        (selector->local-path fixtures-dir "dir-a") )

  (test "selector->local-path allows root-dir to end with a '/'"
        (make-pathname fixtures-dir "dir-a")
        (let ((root-dir (sprintf "~A/" fixtures-dir)))
          (selector->local-path root-dir "dir-a") ) )

  (test "selector->local-path allows root-dir to not end with a '/'"
        (make-pathname fixtures-dir "dir-a")
        (let ((root-dir (string-chomp fixtures-dir "/")))
          (selector->local-path root-dir "dir-a") ) )


  ;; This isn't a good idea but the test ensures that '/' isn't turned into ''
  (test "selector->local-path allows root-dir to be '/'"
        "/"
        (selector->local-path "/" "") )


  (test "selector->local-path trims whitespace and '/' characters from both ends of a selector"
        '(#t #t #t #t #t #t #t #t #t)
        (let ((selectors '("/dir-a" " /dir-a" " / dir-a" "/ dir-a" " dir-a"
                           "dir-a/" "dir-a//" "dir-a / /" " dir-a ")))
            (map (lambda (selector)
                         (equal? (make-pathname fixtures-dir "dir-a")
                                 (selector->local-path fixtures-dir selector)))
                 selectors) ) )


  (test "trim-path-selector removes whitespace and '/' characters at beginning and end of selectors"
        '("hello" "hello" "h ello" "hello" "he  e/llo" "hello" "hello" "hello")
        (let ((test-selectors '("  /hello"
                                "/hello"
                                "  /h ello/  "
                                "/hello/"
                                "  /he  e/llo/  "
                               "//hello//"
                                "\t \t hello\t \t"
                                "\n \n hello\n \n")))
          (map trim-path-selector test-selectors) ) )


  (test "safe-path? returns #t if a path is in root-dir or a sub-directory at any depth"
        '(#t #t #t #t)
        (let ((test-paths '("/tmp" "/tmp/" "/tmp/1/2/3/info.txt" "/tmp/.dotfile")))
          (map (lambda (p) (safe-path? "/tmp" p))
               test-paths) ) )


  (test "safe-path? returns #f root-dir is a relative path"
        '((safe-path? "root-dir must be an absolute directory: ")
          (safe-path? "root-dir must be an absolute directory: tmp/")
          (safe-path? "root-dir must be an absolute directory: ./tmp"))
        (let ((test-roots '("" "tmp/" "./tmp")))
          (map (lambda (r)
                 (handle-exceptions ex
                   (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message))
                   (safe-path? r "/tmp")))
               test-roots) ) )


  (test "safe-path? returns #f path is a relative path"
        '(#f #f #f #f)
        (let ((test-paths '("" "tmp" "tmp/t" "./tmp")))
          (map (lambda (p) (safe-path? "/" p))
               test-paths) ) )


  (test "safe-path? returns #f if a path is outside root-dir"
        '(#f #f #f #f #f #f #f)
        (let ((test-paths '("/"
                            "tmp/"
                            "/tmpsimilardir"
                            "/tmp/../bin"
                            "/tmr"
                            "/tm"
                            "/bin/")))
          (map (lambda (p) (safe-path? "/tmp" p))
               test-paths) ) )


  (test "safe-path? returns #t for any absolute path if root dir is '/'"
        '(#t #t #t #t #t)
        (let ((test-paths '("/"
                            "/tmp"
                            "/tmp/"
                            "/tmp/t"
                            "/tmp/t/")))
          (map (lambda (p) (safe-path? "/" p))
               test-paths) ) )


  (test "safe-path? returns #f if a path contains: '\', './' or '..'"
        '(#f #f #f #f)
        (let ((test-paths '("/tmp\\here"
                            "/tmp./"
                            "/tmp/./"
                            "/tmp/t/..")))
          (map (lambda (p) (safe-path? "/tmp" p))
               test-paths) ) )


  (test "safe-read-file raises an error if trying to serve a file that isn't world readable"
        (list "Hello, this is used to test serving a non world readable file.\n"
              (list 'safe-read-file
                    "can't read file, path isn't word readable: /tmp#t"))
        (let* ((tmpdir (create-temporary-directory))
               (tmpfile (make-pathname tmpdir "hello.txt")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable")
                                    "hello.txt")
                     tmpfile)
          (let ((response1 (safe-read-file 1000 tmpdir tmpfile))
                (exn
                  (handle-exceptions ex
                    (list (get-condition-property ex 'exn 'location)
                          (irregex-replace/all "\/tmp\/.*?hello.txt"
                                               (get-condition-property ex 'exn 'message)
                                               "\/tmp#t"))
                    ;; Make tmpfile non world readable
                    (set-file-permissions! tmpfile
                                           (bitwise-and (file-permissions tmpfile)
                                                        (bitwise-not perm/iroth)))
                    (safe-read-file 1000 tmpdir tmpfile))))
            (list response1 exn) ) ) )


  (test "safe-read-file raises an error if file path isn't safe"
        (list 'safe-read-file
             (sprintf "can't read file, path isn't safe: ~A"
                      (make-pathname fixtures-dir "a.txt")))
        (let ((max-size 5))
          (handle-exceptions ex
                             (list (get-condition-property ex 'exn 'location)
                                   (get-condition-property ex 'exn 'message))
            (safe-read-file max-size
                            (make-pathname fixtures-dir "dir-a")
                            (make-pathname fixtures-dir "a.txt") ) ) ) )


  (test "safe-read-file raises an error if file is bigger than max-size"
        (list 'unsafe-read-file
             (sprintf "can't read file, file is too big: ~A"
                      (make-pathname fixtures-dir "a.txt")))
        (let ((max-size 5))
          (handle-exceptions ex
                             (list (get-condition-property ex 'exn 'location)
                                   (get-condition-property ex 'exn 'message))
            (safe-read-file max-size
                            fixtures-dir
                            (make-pathname fixtures-dir "a.txt") ) ) ) )


  (test "safe-read-file can read a file whose size is equal to max-size"
        "hello\n"
        (let ((max-size 6))
          (safe-read-file max-size
                          fixtures-dir
                          (make-pathname fixtures-dir "a.txt") ) ) )


  (test "safe-read-file returns the contents of a binary file"
        "This is text followed by a null (00)\x00 now some more text."
        (safe-read-file 1000
                        fixtures-dir
                        (make-pathname fixtures-dir "dir-a/ac.bin") ) )


  (test "safe-read-file returns the contents of an empty file"
        ""
        (safe-read-file 1000
                        fixtures-dir
                        (make-pathname fixtures-dir "dir-a/empty.txt") ) )


)

