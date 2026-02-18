;;; Tests for the route handlers

(test-group "handlers"

  (parameterize ((server-hostname "localhost")
                 (server-port 70))

  ;; TODO: Add Test for working through the three handlers in serve-path

  (test "serve-dir Returns Error if listing a directory that isn't world readable"
        (list (string-intersperse '(
                "1dir-a\tdir-a\tlocalhost\t70"
                "1dir-b\tdir-b\tlocalhost\t70"
                ".\r\n")
                "\r\n")
              '("can't list dir, path isn't world readable"
                ((local-path . #t))))
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
            (list (cases Result response1
                    (Ok (v) v)
                    (Error () #f))
                  (cases Result response2
                    (Ok () #f)
                    (Error (msg log-entries)
                      (list msg
                            (confirm-field-matches 'local-path "\/tmp\/.*$" log-entries) ) ) ) ) ) ) )


  (test "serve-dir supportes empty selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
              "1dir-a\tdir-a\tlocalhost\t70"
              "1dir-b\tdir-b\tlocalhost\t70"
              "1dir-world_readable\tdir-world_readable\tlocalhost\t70"
              "0a.txt\ta.txt\tlocalhost\t70"
              "0b.txt\tb.txt\tlocalhost\t70"
              "9noext\tnoext\tlocalhost\t70"
              ".\r\n")
              "\r\n"))
        (serve-dir fixtures-dir (make-request "" "127.0.0.1") ) )


  ;; TODO: test against serve-file as well
  (test "serve-dir supportes subpath ('dir-a') selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
              "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
              "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
              "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
              "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
              ".\r\n")
              "\r\n"))
        (serve-dir fixtures-dir (make-request "dir-a" "127.0.0.1") ) )


  (test "serve-dir returns Not-Applicable if path isn't a directory"
        (Not-Applicable #t)
        (serve-dir fixtures-dir (make-request "a.txt" "127.0.0.1") ) )


  (test "serve-file returns Error if trying to serve a file that isn't world readable"
        '("Hello, this is used to test serving a non world readable file.\n"
          ("can't read file, file path isn't world readable" ((file . #t))))
        (let ((tmpdir (create-temporary-directory))
              (request (make-request "hello.txt" "127.0.0.1")))
          (copy-file (make-pathname (list fixtures-dir "dir-world_readable")
                                    "hello.txt")
                     (make-pathname tmpdir "hello.txt"))
          (let ((response1 (serve-file tmpdir request))
                (response2
                  (begin
                    ;; Make tmpdir non world readable
                    (set-file-permissions! (make-pathname tmpdir "hello.txt")
                                           (bitwise-and (file-permissions tmpdir)
                                                        (bitwise-not perm/iroth)))
                    (serve-file tmpdir request))))
            (list (cases Result response1
                    (Ok (v) v)
                    (Error () #f))
                  (cases Result response2
                    (Ok () #f)
                    (Error (msg log-entries)
                      (list msg
                            (confirm-field-matches 'file ".*?hello.txt$" log-entries) ) ) ) ) ) ) )


  (test "serve-file returns the contents of a binary file as Ok"
        (Ok "This is text followed by a null (00)\x00 now some more text.")
        (serve-file fixtures-dir (make-request "dir-a/ac.bin" "127.0.0.1") ) )


  (test "serve-file returns the contents of an empty file as Ok"
        (Ok "")
        (serve-file fixtures-dir (make-request "dir-a/empty.txt" "127.0.0.1") ) )


  (test "serve-file returns contents of 'index' file if index file requested by selector as Ok"
        (Ok (string-intersperse '(
              "A simple index file to check it can be served without being"
              "processed."
              "=> http://example.com This link line should show the processind =>"
              "")
              "\n"))
        (serve-file fixtures-dir (make-request "dir-b/index" "127.0.0.1") ) )


  (test "serve-file can serve a file that is equal to the number of bytes set by max-response-size"
        (Ok "hello\n")
        (parameterize ((max-response-size 6))
          (serve-file fixtures-dir (make-request "a.txt" "127.0.0.1") ) ) )


  (test "serve-file returns Error if selector isn't safe"
        (Error "path isn't safe"
               (list (cons 'local-path (make-pathname fixtures-dir "../dir-a"))))
        (serve-file fixtures-dir (make-request "../dir-a" "127.0.0.1") ) )


  (test "serve-file returns Not-Applicable if path isn't a regular file"
        (Not-Applicable #t)
        (serve-file fixtures-dir (make-request "dir-b" "127.0.0.1") ) )


  (test "serve-file returns Error if file is bigger than max-response-size"
        (list "hello\n"
              (list "can't read file, file is too big"
                    (list (cons 'file (make-pathname fixtures-dir "a.txt")))))
        (let ((response1 (parameterize ((max-response-size 5000))
                           (cases Result (serve-file fixtures-dir
                                                     (make-request "a.txt" "127.0.0.1"))
                             (Ok (v) v)
                             (Error () #f))))
              (response2 (parameterize ((max-response-size 5))
                           (cases Result (serve-file fixtures-dir
                                                     (make-request "a.txt" "127.0.0.1"))
                             (Ok () #f)
                             (Error (msg log-entries) (list msg log-entries))))))

          (list response1 response2) ) )


  (test "serve-path returns Not-Applicable if path doesn't exist"
        (Not-Applicable #t)
        (serve-path fixtures-dir (make-request "unknown" "127.0.0.1") ) )


  ;; TODO: Add tests to test serve-path going to dir or file

  (test "serve-url returns a HTML document populated with the supplied URL as Ok"
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
          (serve-url (make-request "URL:https://example.com/blog" "127.0.0.1")))


  (test "serve-url returns a HTML document populated with the supplied URL including trailing '/' as Ok"
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
        (serve-url (make-request "URL:https://example.com/blog/" "127.0.0.1")))


  (test "serve-url returns Not-Applicable if selector isn't valid"
        (Not-Applicable #t)
        (serve-url (make-request "FURL:https://example.com/blog" "127.0.0.1")))


) )

