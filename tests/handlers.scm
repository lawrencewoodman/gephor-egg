;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add test for root-dir not existing in case specific incorrectly

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

  (test "selector->local-path returns false if root-dir is a relative dir"
        #f
        (selector->local-path "tests/fixtures" "dir-a") )

  (test "selector->local-path returns false if root-dir is a relative dir of the form ./"
        #f
        (selector->local-path "./" "dir-a") )

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


  ;; TODO: Need to test log messages for this as well
  (test "serve-dir returns false if listing a directory that isn't world readable"
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


  (test "serve-file can serve a file that is equal to the number of bytes set by max-file-size"
        "hello\n"
        (parameterize ((max-file-size 6))
          (serve-file fixtures-dir (make-request "a.txt" "127.0.0.1"))))


  (test "serve-file returns #f and logs an error if file is greater than the number of bytes set by max-file-size"
        (list #f
             (sprintf "ts=#t level=warning msg=\"file is too big to read\" file=~A max-file-size=5\n"
                      (make-pathname fixtures-dir "a.txt")))
        (let ((log-test-port (open-output-string)))
          (parameterize ((max-file-size 5)
                         (log-level 30)
                         (log-port log-test-port))
            (list (serve-file fixtures-dir (make-request "a.txt" "127.0.0.1"))
                  (confirm-log-entries-valid-timestamp (get-output-string log-test-port) ) ) ) ) )


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

