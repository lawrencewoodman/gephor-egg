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

)

