(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken format)
        (chicken file)
        (chicken file posix)
        (chicken load)
        (chicken pathname)
        (chicken process-context)
        (chicken port)
        (chicken string)
        simple-logger)


(load-relative "../server.scm")
(import gophser)

(define dummy)


;; TODO: Test log output
;;(log-level 100)
(log-level 0)


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
        (let* ((menu (list (menu-item 'menu "Somewhere interesting" "/interesting" "localhost" 70))))
          (menu-render menu)))

  (test "make-item-info-wrap wraps info text"
        (string-intersperse '(
          "iThis is some interesting text that you might like to read about if\tFAKE\tlocalhost\t70"
          "iyou have the time to look at it but the main point is that it is\tFAKE\tlocalhost\t70"
          "ithere to see if the text wrapping works properly\tFAKE\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((text (string-intersperse '(
                      "This is some interesting text that you might like to read about if you have the time to look at it but the main"
                      "point is that it is there to see"
                      "if the text wrapping works properly")
                      "\n"))
               (menu (menu-item-info-wrap text "FAKE" "localhost" 70)))
          (menu-render menu)))


  ;; TODO: Will need to add more types as they become supported
  (test "make-item handles a range of types and their human readable names"
        (string-intersperse '(
          "0Some text\ttext text\tlocalhost\t70"
          "0Some text\ttext 0\tlocalhost\t70"
          "1A menu\tmenu menu\tlocalhost\t70"
          "1A menu\tmenu 1\tlocalhost\t70"
          "3An error\terror error\tlocalhost\t70"
          "3An error\terror 3\tlocalhost\t70"
          "4A bin hex file\tbinhex binhex\tlocalhost\t70"
          "4A bin hex file\tbinhex 4\tlocalhost\t70"
          "9A binary file\tbinary binary\tlocalhost\t70"
          "9A binary file\tbinary 9\tlocalhost\t70"
          "iSome info\tinfo info\tlocalhost\t70"
          "iSome info\tinfo i\tlocalhost\t70"
          "hSome html\thtml html\tlocalhost\t70"
          "hSome html\thtml h\tlocalhost\t70"
          "gA Gif\tgif gif\tlocalhost\t70"
          "gA Gif\tgif g\tlocalhost\t70"
          "ISome image\timage image\tlocalhost\t70"
          "ISome image\timage I\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((menu-src '(
                 (text "Some text" "text text" "localhost" 70)
                 (|0| "Some text" "text 0" "localhost" 70)
                 (menu "A menu" "menu menu" "localhost" 70)
                 (|1| "A menu" "menu 1" "localhost" 70)
                 (error "An error" "error error" "localhost" 70)
                 (|3| "An error" "error 3" "localhost" 70)
                 (binhex "A bin hex file" "binhex binhex" "localhost" 70)
                 (|4| "A bin hex file" "binhex 4" "localhost" 70)
                 (binary "A binary file" "binary binary" "localhost" 70)
                 (|9| "A binary file" "binary 9" "localhost" 70)
                 (info "Some info" "info info" "localhost" 70)
                 (i "Some info" "info i" "localhost" 70)
                 (html "Some html" "html html" "localhost" 70)
                 (h "Some html" "html h" "localhost" 70)
                 (gif "A Gif" "gif gif" "localhost" 70)
                 (g "A Gif" "gif g" "localhost" 70)
                 (image "Some image" "image image" "localhost" 70)
                 (I "Some image" "image I" "localhost" 70)))
               (menu (map (lambda (x) (apply menu-item x)) menu-src)))
          (menu-render menu)))


  (test "make-item returns a blank info item if itemtype is unknown"
        (string-intersperse '(
          "i\t\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((menu (list (menu-item 'u "something" "/fred/hi" "localhost" 70))))
          (menu-render menu)))


  (test "make-item-file handles a selector with no file extension"
        (string-intersperse '(
          "0About this Place\tdir-a/about\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((menu (list (menu-item-file "About this Place"
                                          "dir-a/about"
                                          "localhost"
                                          70))))
          (menu-render menu)))


(test "make-item-url handles gopher protocol"
      (string-intersperse '(
        "1A good gopher example\t\texample.com\t70"
        "1A good gopher example\t/\texample.com\t70"
        "1My phlog\t~myuser/phlog\texample.com\t70"
        "0Pondering something really clever\t~myuser/phlog/something-really-clever.txt\texample.com\t70"
        "1My phlog\t/~myuser/phlog\texample.com\t70"
        "0Pondering something really clever\t/~myuser/phlog/something-really-clever.txt\texample.com\t70"
        "1A good gopher example\t\texample.com\t7070"
        "1A good gopher example\t/\texample.com\t7070"
        "1My phlog\t~myuser/phlog\texample.com\t7070"
        "0Pondering something really clever\t~myuser/phlog/something-really-clever.txt\texample.com\t7070"
        "1My phlog\t/~myuser/phlog\texample.com\t7070"
        "0Pondering something really clever\t/~myuser/phlog/something-really-clever.txt\texample.com\t7070"
        ".\r\n")
        "\r\n")
      (let* ((urls '(
               ;; NOTE: checks handles selectors that start with and without a slash
               ("gopher://example.com" . "A good gopher example")
               ("gopher://example.com/" . "A good gopher example")
               ("gopher://example.com/1~myuser/phlog" . "My phlog")
               ("gopher://example.com/0~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
               ("gopher://example.com/1/~myuser/phlog" . "My phlog")
               ("gopher://example.com/0/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
               ("gopher://example.com:7070" . "A good gopher example")
               ("gopher://example.com:7070/" . "A good gopher example")
               ("gopher://example.com:7070/1~myuser/phlog" . "My phlog")
               ("gopher://example.com:7070/0~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
               ("gopher://example.com:7070/1/~myuser/phlog" . "My phlog")
               ("gopher://example.com:7070/0/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")))
             (menu (map (lambda (x)
                          (menu-item-url "localhost" 7071 (cdr x) (car x)))
                         urls)))
        (menu-render menu) ) )


(test "make-item-url handles http protocol"
      (string-intersperse '(
        "hA good http example\tURL:http://example.com\tlocalhost\t70"
        "hA good http example\tURL:http://example.com/\tlocalhost\t70"
        "hMy phlog\tURL:http://example.com/~myuser/phlog\tlocalhost\t70"
        "hPondering something really clever\tURL:http://example.com/~myuser/phlog/something-really-clever.txt\tlocalhost\t70"
        "hA good http example\tURL:http://example.com:8080\tlocalhost\t70"
        "hA good http example\tURL:http://example.com:8080/\tlocalhost\t70"
        "hMy phlog\tURL:http://example.com:8080/~myuser/phlog\tlocalhost\t70"
        "hPondering something really clever\tURL:http://example.com:8080/~myuser/phlog/something-really-clever.txt\tlocalhost\t70"
        ".\r\n")
        "\r\n")
      (let* ((urls '(
               ("http://example.com" . "A good http example")
               ("http://example.com/" . "A good http example")
               ("http://example.com/~myuser/phlog" . "My phlog")
               ("http://example.com/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
               ("http://example.com:8080" . "A good http example")
               ("http://example.com:8080/" . "A good http example")
               ("http://example.com:8080/~myuser/phlog" . "My phlog")
               ("http://example.com:8080/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")))
             (menu (map (lambda (x)
                          (menu-item-url "localhost" 70 (cdr x) (car x)))
                         urls)))
        (menu-render menu) ) )


(test "make-item-url handles https protocol"
      (string-intersperse '(
        "hA good https example\tURL:https://example.com\tlocalhost\t70"
        "hA good https example\tURL:https://example.com/\tlocalhost\t70"
        "hMy phlog\tURL:https://example.com/~myuser/phlog\tlocalhost\t70"
        "hPondering something really clever\tURL:https://example.com/~myuser/phlog/something-really-clever.txt\tlocalhost\t70"
        "hA good https example\tURL:https://example.com:8443\tlocalhost\t70"
        "hA good https example\tURL:https://example.com:8443/\tlocalhost\t70"
        "hMy phlog\tURL:https://example.com:8443/~myuser/phlog\tlocalhost\t70"
        "hPondering something really clever\tURL:https://example.com:8443/~myuser/phlog/something-really-clever.txt\tlocalhost\t70"
        ".\r\n")
        "\r\n")
      (let* ((urls '(
               ("https://example.com" . "A good https example")
               ("https://example.com/" . "A good https example")
               ("https://example.com/~myuser/phlog" . "My phlog")
               ("https://example.com/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
               ("https://example.com:8443" . "A good https example")
               ("https://example.com:8443/" . "A good https example")
               ("https://example.com:8443/~myuser/phlog" . "My phlog")
               ("https://example.com:8443/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")))
             (menu (map (lambda (x)
                          (menu-item-url "localhost" 70 (cdr x) (car x)))
                         urls)))
        (menu-render menu) ) )


(test "make-item-url handles ssh protocol"
      (string-intersperse '(
        "hsome ssh bbs\tURL:ssh://example.com\tlocalhost\t70"
        "hsome ssh bbs\tURL:ssh://example.com/user/bob\tlocalhost\t70"
        "hsome ssh bbs - my user\tURL:ssh://myuser@example.com\tlocalhost\t70"
        "hsome ssh bbs - my user\tURL:ssh://myuser@example.com/user/bob\tlocalhost\t70"
        "hsome ssh bbs\tURL:ssh://example.com:2320\tlocalhost\t70"
        "hsome ssh bbs\tURL:ssh://example.com:2320/user/bob\tlocalhost\t70"
        "hsome ssh bbs - my user\tURL:ssh://myuser@example.com:2320\tlocalhost\t70"
        "hsome ssh bbs - my user\tURL:ssh://myuser@example.com:2320/user/bob\tlocalhost\t70"
        ".\r\n")
        "\r\n")
      (let* ((urls '(
               ("ssh://example.com" . "some ssh bbs")
               ("ssh://example.com/user/bob" . "some ssh bbs")
               ("ssh://myuser@example.com" . "some ssh bbs - my user")
               ("ssh://myuser@example.com/user/bob" . "some ssh bbs - my user")
               ("ssh://example.com:2320" . "some ssh bbs")
               ("ssh://example.com:2320/user/bob" . "some ssh bbs")
               ("ssh://myuser@example.com:2320" . "some ssh bbs - my user")
               ("ssh://myuser@example.com:2320/user/bob" . "some ssh bbs - my user")))
             (menu (map (lambda (x)
                          (menu-item-url "localhost" 70 (cdr x) (car x)))
                         urls)))
        (menu-render menu) ) )


(test "make-item-url handles non lower case protocol"
      (string-intersperse '(
        "1A good gopher example\t\texample.com\t70"
        "hA good http example\tURL:htTP://example.com\tlocalhost\t70"
        ".\r\n")
        "\r\n")
      (let* ((urls '(
               ("GoPher://example.com" . "A good gopher example")
               ("htTP://example.com" . "A good http example")))
             (menu (map (lambda (x)
                          (menu-item-url "localhost" 70 (cdr x) (car x)))
                         urls)))
        (menu-render menu) ) )


  (test "make-item-url returns a blank info item if protocol is unknown"
        (string-intersperse '(
          "i\t\tlocalhost\t7071"
          ".\r\n")
          "\r\n")
        (let ((menu (list (menu-item-url "localhost"
                                         7071
                                         "Something interesting"
                                         "fred://example.com"))))
          (menu-render menu) ) )

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

  (test "serve-path supportes empty selector"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "1dir-a\tdir-a\tlocalhost\t70"
          "1dir-b\tdir-b\tlocalhost\t70"
          "0a.txt\ta.txt\tlocalhost\t70"
          "0b.txt\tb.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path supportes subpath ('dir-a') selector"
        ;; Directories come before regular files and each in alphabetical order
        (string-intersperse '(
          "0aa.txt\tdir-a/aa.txt\tlocalhost\t70"
          "0ab.txt\tdir-a/ab.txt\tlocalhost\t70"
          ;; TODO: Incorrect item type of ac.bin
          "9ac.bin\tdir-a/ac.bin\tlocalhost\t70"
          "9empty.txt\tdir-a/empty.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns an 'invalid selector' error menu if selector contains '..'"
        (string-intersperse '(
          "3invalid selector\t../dir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "../dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns an 'invalid selector' error menu if selector contains './'"
        (string-intersperse '(
          "3invalid selector\t./dir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "./dir-a" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns an 'invalid selector' error menu if selector contains a '\\'"
        (string-intersperse '(
          "3invalid selector\tdir-a\\fred\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a\\fred" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )

  (test "serve-path returns a 'server error' error menu if root-dir ends with a '/'"
        (string-intersperse '(
          "3server error\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir (sprintf "~A/" fixtures-dir)))
          (serve-path context request local-dir) ) )

  (test "serve-path returns a 'server error' error menu if root-dir is a relative dir"
        (string-intersperse '(
          "3server error\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir "fixtures"))
          (serve-path context request local-dir) ) )

  (test "serve-path returns a 'server error' error menu if root-dir contains .."
        (string-intersperse '(
          "3server error\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir "../"))
          (serve-path context request local-dir) ) )

  (test "serve-path returns a 'server error' error menu if root-dir contains ./"
        (string-intersperse '(
          "3server error\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir "./"))
          (serve-path context request local-dir) ) )

  (test "serve-path returns a 'server error' error menu if root-dir contains \\"
        (string-intersperse '(
          "3server error\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a" "127.0.0.1"))
               (local-dir "\\"))
          (serve-path context request local-dir) ) )

  (test "serve-path returns a 'path not found' error menu if path isn't world readable"
        (string-intersperse '(
          "3path not found\t\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (tmpdir (create-temporary-directory))
               (request (make-request "" "127.0.0.1")))
          ;; Make tmpdir non world readable
          (set-file-permissions! tmpdir
                                 (bitwise-and (file-permissions tmpdir)
                                              (bitwise-not perm/iroth)))
          (serve-path context request tmpdir) ) )

  (test "serve-path returns a 'path not found' error menu if path doesn't exist"
        (string-intersperse '(
          "3path not found\tunknown\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "unknown" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path returns the contents of a binary file"
        "This is text followed by a null (00)\x00 now some more text."
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a/ac.bin" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path returns the contents of an empty file"
        ""
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-a/empty.txt" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


  (test "serve-path process 'index' files properly if present"
        ;; Whitespace is stripped at the beginning and end of file
        (string-intersperse '(
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
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "dir-b" "127.0.0.1")))
          (serve-path context request fixtures-dir) ) )


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
        (let* ((context (make-context "localhost" 70))
               (request (make-request "URL:https://example.com/blog" "127.0.0.1")))
          (serve-url context request) ) )


  (test "serve-url returns a 'server error' error menu if selector isn't valid"
        (string-intersperse '(
          "3server error\tFURL:https://example.com/blog\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((context (make-context "localhost" 70))
               (request (make-request "FURL:https://example.com/blog" "127.0.0.1")))
          (serve-url context request) ) )

)

