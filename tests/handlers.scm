;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add Test for working through the three handlers in serve-path

  (test "selector->local-path returns false if selector contains '..'"
        #f
        (selector->local-path fixtures-dir "../dir-a") )

  (test "selector->local-path returns false if selector contains './'"
        #f
        (selector->local-path fixtures-dir "./dir-a") )

  (test "selector->local-path returns false if selector contains a '\\'"
        #f
        (selector->local-path fixtures-dir "dir-a\\fred") )

  (test "selector->local-path returns false if root-dir contains ./"
        #f
        (selector->local-path "/tmp/./this" "dir-a") )

  (test "selector->local-path false if root-dir contains .."
        #f
        (selector->local-path "/.." "dir-a") )

  (test "selector->local-path returns false if root-dir contains \\"
        #f
        (selector->local-path "/\\" "dir-a") )

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


  (test "selector->local-path returns #f and logs a warning if local-path isn't safe"
        (list #f
              "ts=#t level=warning msg=\"path isn't safe\" path=#t connection-id=3\n")
        (let ((log-test-port (open-output-string))
              (selector "../bin"))
          (parameterize ((log-level 30)
                         (log-port log-test-port)
                         (log-context (list (cons 'connection-id 3))))
            (list (selector->local-path fixtures-dir selector)
                  (irregex-replace/all "path=.*?../bin"
                    (confirm-log-entries-valid-timestamp (get-output-string log-test-port))
                    "path=#t") ) ) ) )


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
        (safe-read-file 1000 fixtures-dir (make-pathname fixtures-dir "dir-a/ac.bin") ) )


  (test "safe-read-file returns the contents of an empty file"
        ""
        (safe-read-file 1000
                        fixtures-dir
                        (make-pathname fixtures-dir "dir-a/empty.txt") ) )


  ;; TODO: Need to test log messages for this as well
  (test "serve-dir returns #f if listing a directory that isn't world readable"
        (list (string-intersperse '(
                "1dir-a\tdir-a\tlocalhost\t70"
                "1dir-b\tdir-b\tlocalhost\t70"
                ".\r\n")
                "\r\n")
              #f)
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          (create-directory (make-pathname tmpdir "dir-a"))
          (create-directory (make-pathname tmpdir "dir-b"))
          (let ((response1 (serve-dir tmpdir request))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! tmpdir
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-dir tmpdir request))))
            (list response1 response2) ) ) )


  (test "serve-dir supportes empty selector"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "1dir-a\tdir-a\tlocalhost\t70"
          "1dir-b\tdir-b\tlocalhost\t70"
          "1dir-world_readable\tdir-world_readable\tlocalhost\t70"
          "0a.txt\ta.txt\tlocalhost\t70"
          "0b.txt\tb.txt\tlocalhost\t70"
          "9noext\tnoext\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (serve-dir fixtures-dir (make-request "" "127.0.0.1") ) )


  ;; TODO: test against serve-file as well
  (test "serve-dir supportes subpath ('dir-a') selector"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (serve-dir fixtures-dir (make-request "dir-a" "127.0.0.1") ) )



  (test "serve-file returns false if trying to serve a file that isn't world readable"
        (list "Hello, this is used to test serving a non world readable file.\n"
              #f)
        (let* ((tmpdir (create-temporary-directory))
               (request (make-request "hello.txt" "127.0.0.1")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable") "hello.txt")
                     (make-pathname tmpdir "hello.txt"))
          (let ((response1 (serve-file tmpdir request))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! (make-pathname tmpdir "hello.txt")
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-file tmpdir request))))
            (list response1 response2) ) ) )


  (test "serve-file returns the contents of a binary file"
        "This is text followed by a null (00)\x00 now some more text."
        (serve-file fixtures-dir (make-request "dir-a/ac.bin" "127.0.0.1") ) )


  (test "serve-file returns the contents of an empty file"
        ""
        (serve-file fixtures-dir (make-request "dir-a/empty.txt" "127.0.0.1")))


  (test "serve-file return contents of 'index' file if index file requested by selector"
        (string-intersperse '(
          "A simple index file to check it can be served without being"
          "processed."
          "=> http://example.com This link line should show the processind =>"
          "")
          "\n")
        (serve-file fixtures-dir (make-request "dir-b/index" "127.0.0.1")))


  (test "serve-file can serve a file that is equal to the number of bytes set by max-response-size"
        "hello\n"
        (parameterize ((max-response-size 6))
          (serve-file fixtures-dir (make-request "a.txt" "127.0.0.1") ) ) )


  (test "serve-file returns #f if selector isn't safe"
        #f
        (serve-file fixtures-dir (make-request "../dir-a" "127.0.0.1") ) )


  (test "serve-file returns false if path isn't a regular file"
        #f
        (serve-file fixtures-dir (make-request "dir-b" "127.0.0.1")))


  (test "serve-file returns #f if can't read file"
        (list "hello\n" #f)
        (let ((response1 (parameterize ((max-response-size 5000))
                           (serve-file fixtures-dir
                                       (make-request "a.txt" "127.0.0.1"))))
              (response2 (parameterize ((max-response-size 5))
                           (serve-file fixtures-dir
                                       (make-request "a.txt" "127.0.0.1")))))
          (list response1 response2) ) )


  (test "serve-path returns false if path doesn't exist"
        #f
        (serve-path fixtures-dir (make-request "unknown" "127.0.0.1")))


  (test "serve-url returns a HTML document populated with the supplied URL"
        (string-intersperse '(
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
          "\n")
          (serve-url (make-request "URL:https://example.com/blog" "127.0.0.1")))


  (test "serve-url returns a HTML document populated with the supplied URL including trailing '/'"
        (string-intersperse '(
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
          "\n")
        (serve-url (make-request "URL:https://example.com/blog/" "127.0.0.1")))


  (test "serve-url returns false if selector isn't valid"
        #f
        (serve-url (make-request "FURL:https://example.com/blog" "127.0.0.1")))


) )

