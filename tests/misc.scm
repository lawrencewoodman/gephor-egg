;;; Tests for the misc procedures


(test-group "misc"

  (test "selector->local-path returns Error if selector contains '..'"
        (Error "path isn't safe"
               (list (cons 'local-path (make-pathname fixtures-dir "../dir-a"))))
        (selector->local-path fixtures-dir "../dir-a") )


  (test "selector->local-path returns Error if selector contains './'"
        (Error "path isn't safe"
               (list (cons 'local-path (make-pathname fixtures-dir "./dir-a"))))
        (selector->local-path fixtures-dir "./dir-a") )


  (test "selector->local-path returns Error if selector contains a '\\'"
        (Error "path isn't safe"
               (list (cons 'local-path (make-pathname fixtures-dir "dir-a\\fred"))))
        (selector->local-path fixtures-dir "dir-a\\fred") )


  (test "selector->local-path returns Error if root-dir contains ./"
        (Error "path isn't safe"
               (list (cons 'local-path "/tmp/./this/dir-a")))
        (selector->local-path "/tmp/./this" "dir-a") )


  (test "selector->local-path returns Error if root-dir contains .."
        (Error "path isn't safe"
              (list (cons 'local-path "/../dir-a")))
        (selector->local-path "/.." "dir-a") )


  (test "selector->local-path returns Error if root-dir contains \\"
        (Error "path isn't safe"
               (list (cons 'local-path "/\\/dir-a")))
        (selector->local-path "/\\" "dir-a") )


  (test "selector->local-path doesn't allow percent decoding to turn %2E%2E into .."
        (Ok (make-pathname fixtures-dir "dir-a/%2E%2E"))
        (selector->local-path fixtures-dir "dir-a/%2E%2E") )


  (test "selector->local-path forms a local-path from root-dir and selector as Ok"
        (Ok (make-pathname fixtures-dir "dir-a"))
        (selector->local-path fixtures-dir "dir-a") )


  (test "selector->local-path allows root-dir to end with a '/'"
        (make-pathname fixtures-dir "dir-a")
        (let ((root-dir (sprintf "~A/" fixtures-dir)))
          (cases Result (selector->local-path root-dir "dir-a")
            (Ok (v) v)
            (Error () #f) ) ) )


  (test "selector->local-path allows root-dir to not end with a '/'"
        (Ok (make-pathname fixtures-dir "dir-a"))
        (let ((root-dir (string-chomp fixtures-dir "/")))
          (selector->local-path root-dir "dir-a") ) )


  ;; It isn't a good idea to use this as the root-dir but the test ensures that '/' isn't turned into ''
  (test "selector->local-path allows root-dir to be '/'"
        (Ok "/")
        (selector->local-path "/" "") )


  (test "selector->local-path trims whitespace and '/' characters from both ends of a selector"
        '(#t #t #t #t #t #t #t #t #t)
        (let ((selectors '("/dir-a" " /dir-a" " / dir-a" "/ dir-a" " dir-a"
                           "dir-a/" "dir-a//" "dir-a / /" " dir-a ")))
            (map (lambda (selector)
                         (cases Result (selector->local-path fixtures-dir selector)
                           (Ok (v) (equal? v (make-pathname fixtures-dir "dir-a")))
                           (Error () #f)))
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


  (test "safe-read-file returns Error if trying to serve a file that isn't world readable"
        '("Hello, this is used to test serving a non world readable file.\n"
          ("can't read file, file path isn't world readable" ((file . #t))))
        (let* ((tmpdir (create-temporary-directory))
               (tmpfile (make-pathname tmpdir "hello.txt")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable")
                                    "hello.txt")
                     tmpfile)
          (let ((response1 (safe-read-file 1000 tmpdir tmpfile))
                (response2
                  (begin
                    ;; Make tmpfile non world readable
                    (set-file-permissions! tmpfile
                                           (bitwise-and (file-permissions tmpfile)
                                                        (bitwise-not perm/iroth)))
                    (safe-read-file 1000 tmpdir tmpfile))))
            (list (cases Result response1 (Ok (v) v) (Error () #f))
                  (cases Result response2
                    (Ok () #f)
                    (Error (msg log-entries)
                      (list msg (confirm-field-matches 'file
                                                      ".*?hello.txt$"
                                                      log-entries) ) ) ) ) ) ) )


  (test "safe-read-file returns Error if file path isn't safe"
    (Error "can't read file, file path isn't safe"
           (list (cons 'file (make-pathname fixtures-dir "a.txt"))))
        (let* ((max-size 5))
          (safe-read-file max-size (make-pathname fixtures-dir "dir-a")
                                   (make-pathname fixtures-dir "a.txt") ) ) )


  (test "safe-read-file returns Error if file is bigger than max-size"
    (Error "can't read file, file is too big"
           (list (cons 'file (make-pathname fixtures-dir "a.txt"))))
          (let* ((max-size 5))
            (safe-read-file max-size fixtures-dir
                                     (make-pathname fixtures-dir "a.txt") ) ) )


  (test "safe-read-file can read a file whose size is equal to max-size"
        (Ok "hello\n")
        (let ((max-size 6))
          (safe-read-file max-size fixtures-dir
                                   (make-pathname fixtures-dir "a.txt") ) ) )


  (test "safe-read-file returns the contents of a binary file as Ok"
        (Ok "This is text followed by a null (00)\x00 now some more text.")
        (safe-read-file 1000 fixtures-dir
                             (make-pathname fixtures-dir "dir-a/ac.bin") ) )


  (test "safe-read-file returns the contents of an empty file as Ok"
        (Ok "")
        (safe-read-file 1000 fixtures-dir
                             (make-pathname fixtures-dir "dir-a/empty.txt") ) )


)

