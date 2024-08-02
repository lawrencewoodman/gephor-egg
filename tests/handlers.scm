;;; Tests for the route handlers

(test-group "handlers"

  (test "serve-path supportes empty selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "1dir-a\tdir-a\tlocalhost\t70"
          "1dir-b\tdir-b\tlocalhost\t70"
          "1dir-index_invalid_url_protocol\tdir-index_invalid_url_protocol\tlocalhost\t70"
          "0a.txt\ta.txt\tlocalhost\t70"
          "0b.txt\tb.txt\tlocalhost\t70"
          "9noext\tnoext\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path supportes subpath ('dir-a') selector"
        ;; Directories come before regular files and each in alphabetical order
        (Ok (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          ;; TODO: Incorrect item type of ac.bin
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path returns an 'invalid selector' error menu if selector contains '..'"
        (Ok (string-intersperse '(
          "3invalid selector\t../dir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "../dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns an 'invalid selector' error menu if selector contains './'"
        (Ok (string-intersperse '(
          "3invalid selector\t./dir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "./dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns an 'invalid selector' error menu if selector contains a '\\'"
        (Ok (string-intersperse '(
          "3invalid selector\tdir-a\\fred\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a\\fred" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path raises an exception if root-dir ends with a '/'"
        (list 'serve-path (sprintf "root-dir isn't valid: ~A/" fixtures-dir))
        (let ((context (make-context "localhost" 70))
              (request (make-request "dir-a" "127.0.0.1"))
              (local-dir (sprintf "~A/" fixtures-dir)))
          (condition-case (serve-path context request local-dir)
            (ex () (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message) ) ) ) ) )


  (test "serve-path raises an exception if root-dir is a relative dir"
        (list 'serve-path "root-dir isn't valid: fixtures")
        (let ((context (make-context "localhost" 70))
              (request (make-request "dir-a" "127.0.0.1"))
              (local-dir "fixtures"))
          (condition-case (serve-path context request local-dir)
            (ex () (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message) ) ) ) ) )

  (test "serve-path raises an exception if root-dir is a relative dir of the form ./"
        (list 'serve-path "root-dir isn't valid: ./")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir "./"))
          (condition-case (serve-path context request local-dir)
            (ex () (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message) ) ) ) ) )

  (test "serve-path raises an exception if root-dir contains .."
        (list 'serve-path "root-dir isn't valid: /..")
        (let ((context (make-context "localhost" 70))
              (request (make-request "dir-a" "127.0.0.1"))
              (local-dir "/.."))
          (condition-case (serve-path context request local-dir)
            (ex () (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message) ) ) ) ) )

  (test "serve-path raises an exception if root-dir contains \\"
        (list 'serve-path "root-dir isn't valid: /\\")
        (let ((context (make-context "localhost" 70))
              (request (make-request "dir-a" "127.0.0.1"))
              (local-dir "/\\"))
          (condition-case (serve-path context request local-dir)
            (ex () (list (get-condition-property ex 'exn 'location)
                         (get-condition-property ex 'exn 'message) ) ) ) ) )


  (test "serve-path returns a 'path not found' error menu if path isn't world readable"
        (Ok (string-intersperse '(
          "3path not found\t\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          ;; Make tmpdir non world readable
          (set-file-permissions! tmpdir
                                 (bitwise-and (file-permissions tmpdir)
                                              (bitwise-not perm/iroth)))
          (serve-path context request tmpdir) ) )


  (test "serve-path returns a 'path not found' error menu if path doesn't exist"
        (Ok (string-intersperse '(
          "3path not found\tunknown\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "unknown" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path returns the contents of a binary file"
        (Ok "This is text followed by a null (00)\x00 now some more text.")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a/ac.bin" "127.0.0.1")))
          (serve-path context request fixtures-dir)))


  (test "serve-path returns the contents of an empty file"
        (Ok "")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a/empty.txt" "127.0.0.1")))
          (serve-path context request fixtures-dir)))


  (test "serve-path process 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        ;; TODO: Can we just compare against Ok
        (Ok (string-intersperse '(
          "i---[[[ Some Title ]]]---\tdir-b\tlocalhost\t70"
          "i\tdir-b\tlocalhost\t70"
          "i\tdir-b\tlocalhost\t70"
          "iLets try out some links.  First some absolute links\tdir-b\tlocalhost\t70"
          "1Back to the beginning\t\tlocalhost\t70"
          "1/dir-a\tdir-a\tlocalhost\t70"
          "0/a.txt\ta.txt\tlocalhost\t70"
          "i\tdir-b\tlocalhost\t70"
          "iNow some relative links\tdir-b\tlocalhost\t70"
          "1dir-ba\tdir-b/dir-ba\tlocalhost\t70"
          "1The ba directory\tdir-b/dir-ba\tlocalhost\t70"
          "1The bb directory\tdir-b/dir-bb\tlocalhost\t70"
          "0dir-ba/baa.txt\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
          "9dir-ba/bac.bin\tdir-b/dir-ba/bac.bin\tlocalhost\t70"
          "i\tdir-b\tlocalhost\t70"
          "iSome URLs used as links\tdir-b\tlocalhost\t70"
          "hhttp://example.com\tURL:http://example.com\tlocalhost\t70"
          "hhttp://example.com/fred\tURL:http://example.com/fred\tlocalhost\t70"
          "hhttp://example.com/fred/\tURL:http://example.com/fred/\tlocalhost\t70"
          "hFred's things\tURL:http://example.com/fred\tlocalhost\t70"
          "i\tdir-b\tlocalhost\t70"
          "iThis file ends with two blank lines which should be stripped.\tdir-b\tlocalhost\t70"
          ".\r\n")
          "\r\n"))
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-b" "127.0.0.1")))
          (serve-path context request fixtures-dir)))


  (test "serve-path raises Error containing a chain of Errors if problem with serving a directory"
        (list 'ok (sprintf "local-path: ~A, error serving directory"
                           (make-pathname fixtures-dir "dir-index_invalid_url_protocol"))
                  "error processing index"
                  "url: fred://example.com, unsupported protocol: fred")
        (let ((context (make-context "localhost" 70))
              (request (make-request "dir-index_invalid_url_protocol" "127.0.0.1")))
          (cases Result (serve-path context request fixtures-dir)
            (Error (e) (cons 'ok e) ) ) ) )


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
        (let* ((context (make-context "localhost" 70))
               (request (make-request "URL:https://example.com/blog" "127.0.0.1")))
          (serve-url context request) ) )


  (test "serve-url returns a 'server error' error menu if selector isn't valid"
        "invalid selector: FURL:https://example.com/blog"
        (let* ((context (make-context "localhost" 70))
               (request (make-request "FURL:https://example.com/blog" "127.0.0.1")))
          (condition-case (serve-url context request)
            (ex () (get-condition-property ex 'exn 'message) ) ) ) )

)

