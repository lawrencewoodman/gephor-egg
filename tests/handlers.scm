;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add test for root-dir not existing in case specific incorrectly

  (test "serve-path supportes empty selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "1dir-a\tdir-a\tlocalhost\t70"
          "1dir-b\tdir-b\tlocalhost\t70"
          "1dir-index_empty_file\tdir-index_empty_file\tlocalhost\t70"
          "1dir-index_world_readable\tdir-index_world_readable\tlocalhost\t70"
          "1dir-world_readable\tdir-world_readable\tlocalhost\t70"
          "0a.txt\ta.txt\tlocalhost\t70"
          "0b.txt\tb.txt\tlocalhost\t70"
          "9noext\tnoext\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (serve-path (make-request "" "127.0.0.1") fixtures-dir) )


  (test "serve-path supportes subpath ('dir-a') selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (serve-path (make-request "dir-a" "127.0.0.1") fixtures-dir) )


  (test "serve-path allows root-dir to end with a '/'"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let ((request (make-request "dir-a" "127.0.0.1"))
              (root-dir (sprintf "~A/" fixtures-dir)))
          (serve-path request root-dir) ) )

  (test "serve-path allows root-dir to not end with a '/'"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let ((request (make-request "dir-a" "127.0.0.1"))
              (root-dir (string-chomp fixtures-dir "/")))
          (serve-path request root-dir) ) )


  ;; This isn't a good idea but the test ensures that '/' isn't turned into ''
  (test "serve-path allows root-dir to be '/'"
        '(0 0 0 0 0 0)
        (cases Result (serve-path (make-request "" "127.0.0.1") "/")
          (Ok (v) (filter-map (lambda (x) (or (substring-index "1bin\t" x)
                                              (substring-index "1dev\t" x)
                                              (substring-index "1home\t" x)
                                              (substring-index "1sbin\t" x)
                                              (substring-index "1usr\t" x)
                                              (substring-index "1var\t" x)))
                              (string-split v "\r\n")))
          (Error (e) e) ) )

  (test "serve-path trims whitespace and '/' characters from both ends of a selector"
        '(#t #t #t #t #t #t #t #t #t)
        (let ((expect (Ok (string-intersperse '(
                            ;; Directories come before regular files and each in alphabetical order
                            "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
                            "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
                            "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
                            "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
                            ".\r\n")
                            "\r\n")))
             (selectors '("/dir-a" " /dir-a" " / dir-a" "/ dir-a" " dir-a"
                          "dir-a/" "dir-a//" "dir-a / /" " dir-a ")))
        (map (lambda (selector)
               (equal? expect
                       (serve-path (make-request selector "127.0.0.1") fixtures-dir)))
             selectors) ) )


  (test "serve-path returns false if selector contains '..'"
        #f
        (serve-path (make-request "../dir-a" "127.0.0.1") fixtures-dir) )

  (test "serve-path returns false if selector contains './'"
        #f
        (serve-path (make-request "./dir-a" "127.0.0.1") fixtures-dir) )

  (test "serve-path returns false if selector contains a '\\'"
        #f
        (serve-path (make-request "dir-a\\fred" "127.0.0.1") fixtures-dir) )

  (test "serve-path doesn't allow percent decoding to allow .."
        #f
        (serve-path (make-request "dir-a/%2E%2E" "127.0.0.1") fixtures-dir) )

  (test "serve-path returns false if root-dir is a relative dir"
        #f
        (serve-path (make-request "dir-a" "127.0.0.1") "tests/fixtures") )

  (test "serve-path returns false if root-dir is a relative dir of the form ./"
        #f
        (serve-path (make-request "dir-a" "127.0.0.1") "./") )

  (test "serve-path returns false if root-dir contains ./"
        #f
        (serve-path (make-request "dir-a" "127.0.0.1") "/tmp/./this") )

  (test "serve-path false if root-dir contains .."
        #f
        (serve-path (make-request "dir-a" "127.0.0.1") "/..") )

  (test "serve-path returns false if root-dir contains \\"
        #f
        (serve-path (make-request "dir-a" "127.0.0.1") "/\\") )


  (test "serve-path returns false if trying to serve a file that isn't world readable"
        (list (list 'ok "Hello, this is used to test serving a non world readable file.\n")
              #f)
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "hello.txt" "127.0.0.1")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable") "hello.txt")
                     (make-pathname tmpdir "hello.txt"))
          (let ((response1 (serve-path request tmpdir))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! (make-pathname tmpdir "hello.txt")
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-path request tmpdir))))
            (list (cases Result response1
                         (Ok (v) (list 'ok v)))
                  response2) ) ) )


  ;; TODO: Should this return false?
  (test "serve-path returns Error if listing a directory that isn't world readable"
        (list (list 'ok (string-intersperse '(
                          "1dir-a\tdir-a\tlocalhost\t70"
                          "1dir-b\tdir-b\tlocalhost\t70"
                          ".\r\n")
                          "\r\n"))
              (list 'error (list "local-path: x/, error serving directory"
                                 "local-path: x/, isn't world readable")))
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          (create-directory (make-pathname tmpdir "dir-a"))
          (create-directory (make-pathname tmpdir "dir-b"))
          (let ((response1 (serve-path request tmpdir))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! tmpdir
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-path request tmpdir))))
            (list (cases Result response1
                         (Ok (v) (list 'ok v)))
                  (cases Result response2
                        (Error (e) (list 'error
                                         (list (irregex-replace "local-path: .*\/"
                                                                (first e)
                                                                "local-path: x/")
                                               (irregex-replace "local-path: .*\/"
                                                                (second e)
                                                                "local-path: x/") ) ) ) ) ) ) ) )


  (test "serve-path returns false if path doesn't exist"
        #f
        (serve-path (make-request "unknown" "127.0.0.1") fixtures-dir) )


  (test "serve-path returns the contents of a binary file"
        (Ok "This is text followed by a null (00)\x00 now some more text.")
        (serve-path (make-request "dir-a/ac.bin" "127.0.0.1") fixtures-dir) )


  (test "serve-path returns the contents of an empty file"
        (Ok "")
        (serve-path (make-request "dir-a/empty.txt" "127.0.0.1") fixtures-dir) )


  (test "serve-path process 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        (Ok (string-intersperse '(
          "iA simple index file to check it is interpreted by serve-path\tdir-b\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (serve-path (make-request "dir-b" "127.0.0.1") fixtures-dir) )


  (test "serve-path process empty 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        (Ok (string-intersperse '(
          "i\tdir-index_empty_file\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (serve-path (make-request "dir-index_empty_file" "127.0.0.1")
                    fixtures-dir) )


  (test "serve-path lists the directory if an 'index' file isn't world readable"
        (list (list 'ok (string-intersperse '(
                          "iThis is used to test an index file that isn't world readable\t\tlocalhost\t70"
                          ".\r\n")
                          "\r\n"))
              (list 'ok (string-intersperse '(
                          "1dir-a\tdir-a\tlocalhost\t70"
                          "1dir-b\tdir-b\tlocalhost\t70"
                          "0index\tindex\tlocalhost\t70"
                          ".\r\n")
                          "\r\n")))
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          (create-directory (make-pathname tmpdir "dir-a"))
          (create-directory (make-pathname tmpdir "dir-b"))
          (copy-file (make-pathname (list fixtures-dir "dir-index_world_readable") "index")
                     (make-pathname tmpdir "index"))
          (let ((response1 (serve-path request tmpdir))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! (make-pathname tmpdir "index")
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-path request tmpdir))))
            (list (cases Result response1
                         (Ok (v) (list 'ok v)))
                  (cases Result response2
                         (Ok (v) (list 'ok v) ) ) ) ) ) )


  (test "serve-path can serve a file that is equal to the number of bytes set by max-file-size"
        (Ok "hello\n")
        (parameterize ((max-file-size 6))
          (serve-path (make-request "a.txt" "127.0.0.1") fixtures-dir) ) )


  (test "serve-path raises an exception if file is greater than the number of bytes set by max-file-size"
        (sprintf "file: ~A, is greater than 5 bytes"
                 (make-pathname fixtures-dir "a.txt"))
        (parameterize ((max-file-size 5))
          (condition-case (serve-path (make-request "a.txt" "127.0.0.1") fixtures-dir)
            (ex () (get-condition-property ex 'exn 'message) ) ) ) )


  (test "serve-url returns a HTML document populated with the supplied URL"
        (Ok (string-intersperse '(
          "<HTML>"
          "  <HEAD>"
          "    <META HTTP-EQUIV=\"refresh\" content=\"2;URL=https://example.com/blog\">"
          "  </HEAD>"
          "  <BODY>"
          "    You are following a link from gopher to a web site.  You will be"
          "    automatically taken to the web site shortly.  If you do not get sent"
          "    there, please click"
          "    <A HREF=\"https://example.com/blog\">here</A> to go to the web site."
          "    <P>"
          "    The URL linked is:"
          "    <P>"
          "    <A HREF=\"https://example.com/blog\">https://example.com/blog</A>"
          "    <P>"
          "    Thanks for using gopher!"
          "  </BODY>"
          "</HTML>")
          "\n"))
        (serve-url (make-request "URL:https://example.com/blog" "127.0.0.1") ) )


  (test "serve-url returns a HTML document populated with the supplied URL including trailing '/'"
        (Ok (string-intersperse '(
          "<HTML>"
          "  <HEAD>"
          "    <META HTTP-EQUIV=\"refresh\" content=\"2;URL=https://example.com/blog/\">"
          "  </HEAD>"
          "  <BODY>"
          "    You are following a link from gopher to a web site.  You will be"
          "    automatically taken to the web site shortly.  If you do not get sent"
          "    there, please click"
          "    <A HREF=\"https://example.com/blog/\">here</A> to go to the web site."
          "    <P>"
          "    The URL linked is:"
          "    <P>"
          "    <A HREF=\"https://example.com/blog/\">https://example.com/blog/</A>"
          "    <P>"
          "    Thanks for using gopher!"
          "  </BODY>"
          "</HTML>")
          "\n"))
        (serve-url (make-request "URL:https://example.com/blog/" "127.0.0.1") ) )


  (test "serve-url returns false if selector isn't valid"
        #f
        (serve-url (make-request "FURL:https://example.com/blog" "127.0.0.1") ) )


) )

