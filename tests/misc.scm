;;; Tests for the misc procedures


(test-group "misc"

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


  (test "safe-read-file returns #f and logs an error if trying to serve a file that isn't world readable"
        (list "Hello, this is used to test serving a non world readable file.\n"
              #f
              "ts=#t level=error msg=\"file isn't world readable\" file=#t connection-id=3\n")
        (let* ((log-test-port (open-output-string))
               (tmpdir (create-temporary-directory))
               (tmpfile (make-pathname tmpdir "hello.txt")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable") "hello.txt")
                     tmpfile)
          (parameterize ((log-level 30)
                         (log-port log-test-port)
                         (log-context (list (cons 'connection-id 3))))
            (let ((response1 (safe-read-file 1000 tmpdir tmpfile))
                  (response2
                    (begin
                      ;; Make tmpfile non world readable
                      (set-file-permissions! tmpfile
                                             (bitwise-and (file-permissions tmpfile)
                                                          (bitwise-not perm/iroth)))
                      (safe-read-file 1000 tmpdir tmpfile))))
              (list response1
                    response2
                    (irregex-replace/all "file=.*?hello.txt"
                      (confirm-log-entries-valid-timestamp (get-output-string log-test-port))
                      "file=#t") ) ) ) ) )


  (test "safe-read-file returns #f and logs an error if file path isn't safe"
        (list #f
             (sprintf "ts=#t level=error msg=\"file path isn't safe\" file=~A connection-id=3\n"
                      (make-pathname fixtures-dir "a.txt")))
        (let ((log-test-port (open-output-string))
              (max-size 5))
          (parameterize ((log-level 30)
                         (log-port log-test-port)
                         (log-context (list (cons 'connection-id 3))))
            (list (safe-read-file max-size
                                  (make-pathname fixtures-dir "dir-a")
                                  (make-pathname fixtures-dir "a.txt"))
                  (confirm-log-entries-valid-timestamp (get-output-string log-test-port) ) ) ) ) )


  (test "safe-read-file returns #f and logs an error if file is greater than max-size"
        (list #f
             (sprintf "ts=#t level=error msg=\"file is too big to read\" file=~A max-size=5 connection-id=3\n"
                      (make-pathname fixtures-dir "a.txt")))
        (let ((log-test-port (open-output-string))
              (max-size 5))
          (parameterize ((log-level 30)
                         (log-port log-test-port)
                         (log-context (list (cons 'connection-id 3))))
            (list (safe-read-file max-size
                                  fixtures-dir
                                  (make-pathname fixtures-dir "a.txt"))
                  (confirm-log-entries-valid-timestamp (get-output-string log-test-port) ) ) ) ) )


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

