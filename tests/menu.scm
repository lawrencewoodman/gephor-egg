;;; Tests for the menu procedures

(test-group "menu"

  ;; The path of the fixtures directory
  ;; TODO: This is repeated for handlers, put in one place
  (define fixtures-dir
    (let loop ((dirs (list (current-directory) (make-pathname (current-directory) "tests"))))
      (if (null? dirs) (error "can't find fixtures directory"))
      (let ((try-path (make-pathname (car dirs) "fixtures")))
        (if (and (file-exists? try-path) (directory? try-path))
            try-path
            (loop (cdr dirs))))))



  (test "make-render adds correct .<cr><lf> to end of menu"
        (string-intersperse '(
          "1Somewhere interesting\t/interesting\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let* ((menu (list (menu-item 'menu "Somewhere interesting" "/interesting" "localhost" 70))))
          (menu-render menu)))


  ;; TODO: Will need to add more types as they become supported
  (test "menu-item handles a range of types and their human readable names"
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
               (menu (map (lambda (x) (apply menu-item x))
                          menu-src)))
          (menu-render menu) ) )


  (test "menu-item allows a single letter itemtype if unknown"
        (string-intersperse '(
          "usomething\t/fred/hi\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((menu (list (menu-item 'u "something" "/fred/hi" "localhost" 70))))
          (menu-render menu) ) )


  (test "menu-item returns #f if unknown itemtype is longer than a single letter"
        #f
        (menu-item 'uu "something" "/fred/hi" "localhost" 70) )


  (test "menu-item allows username > 69 despite warning"
        (string-intersperse (list
          (sprintf "i~A\t\tlocalhost\t70" (make-string 68 #\a))
          (sprintf "i~A\t\tlocalhost\t70" (make-string 69 #\a))
          (sprintf "i~A\t\tlocalhost\t70" (make-string 70 #\a))
          (sprintf "i~A\t\tlocalhost\t70" (make-string 71 #\a))
          ".\r\n")
          "\r\n")
        (let* ((lengths '(68 69 70 71))
               (menu (map (lambda (l)
                            (menu-item 'info (make-string l #\a) "" "localhost" 70))
                          lengths)))
          (menu-render menu) ) )


  (test "menu-item logs a warning if username > 69 characters but uses anyway"
        (string-intersperse (list
          (sprintf "ts=#t level=warning msg=\"username > 69 characters\" username=~A selector=\"\" hostname=localhost port=70"
                   (make-string 70 #\a))
          (sprintf "ts=#t level=warning msg=\"username > 69 characters\" username=~A selector=\"\" hostname=localhost port=70\n"
                   (make-string 71 #\a)))
          "\n")
        (let ((lengths '(68 69 70 71))
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (log-port port))
            (for-each (lambda (l)
                        (menu-item 'info (make-string l #\a) "" "localhost" 70))
                      lengths)
            (confirm-log-entries-valid-timestamp (get-output-string port) ) ) ) )


  (test "menu-item-file handles a selector with no file extension"
        (string-intersperse '(
          "9A file with no extension\tnoext\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (parameterize ((server-hostname "localhost") (server-port 70))
          (let ((menu (list (menu-item-file (make-pathname fixtures-dir "noext")
                                            "A file with no extension"
                                            "noext"))))
            (menu-render menu) ) ) )


  (test "menu-item-file returns #f if the file doesn't exist"
        #f
        (parameterize ((server-hostname "localhost") (server-port 70))
          (menu-item-file (make-pathname fixtures-dir "nonexistent.txt")
                          "A file that doesn't exist"
                          "nonexistent.txt") ) )


  (test "menu-item-file detects directories properly"
        (string-intersperse '(
          "1A directory\tdir\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (parameterize ((server-hostname "localhost") (server-port 70))
          (let ((menu (list (menu-item-file (make-pathname fixtures-dir "dir-world_readable")
                                            "A directory"
                                            "dir"))))
            (menu-render menu) ) ) )


(test "menu-item-url handles gopher protocol"
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
      (parameterize ((server-hostname "localhost") (server-port 7071))
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
               (menu (map (lambda (x) (menu-item-url (cdr x) (car x)))
                          urls)))
          (menu-render menu) ) ) )


(test "menu-item-url handles http protocol"
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
      (parameterize ((server-hostname "localhost") (server-port 70))
        (let* (
               (urls '(
                 ("http://example.com" . "A good http example")
                 ("http://example.com/" . "A good http example")
                 ("http://example.com/~myuser/phlog" . "My phlog")
                 ("http://example.com/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
                 ("http://example.com:8080" . "A good http example")
                 ("http://example.com:8080/" . "A good http example")
                 ("http://example.com:8080/~myuser/phlog" . "My phlog")
                 ("http://example.com:8080/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")))
               (menu (map (lambda (x) (menu-item-url (cdr x) (car x)))
                          urls)))
          (menu-render menu) ) ) )


(test "menu-item-url handles https protocol"
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
      (parameterize ((server-hostname "localhost") (server-port 70))
        (let* ((urls '(
                 ("https://example.com" . "A good https example")
                 ("https://example.com/" . "A good https example")
                 ("https://example.com/~myuser/phlog" . "My phlog")
                 ("https://example.com/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")
                 ("https://example.com:8443" . "A good https example")
                 ("https://example.com:8443/" . "A good https example")
                 ("https://example.com:8443/~myuser/phlog" . "My phlog")
                 ("https://example.com:8443/~myuser/phlog/something-really-clever.txt" . "Pondering something really clever")))
               (menu (map (lambda (x) (menu-item-url (cdr x) (car x)))
                          urls)))
          (menu-render menu) ) ) )


(test "menu-item-url handles ssh protocol"
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
      (parameterize ((server-hostname "localhost") (server-port 70))
        (let* ((urls '(
                 ("ssh://example.com" . "some ssh bbs")
                 ("ssh://example.com/user/bob" . "some ssh bbs")
                 ("ssh://myuser@example.com" . "some ssh bbs - my user")
                 ("ssh://myuser@example.com/user/bob" . "some ssh bbs - my user")
                 ("ssh://example.com:2320" . "some ssh bbs")
                 ("ssh://example.com:2320/user/bob" . "some ssh bbs")
                 ("ssh://myuser@example.com:2320" . "some ssh bbs - my user")
                 ("ssh://myuser@example.com:2320/user/bob" . "some ssh bbs - my user")))
               (menu (map (lambda (x) (menu-item-url (cdr x) (car x)))
                          urls)))
          (menu-render menu) ) ) )


(test "menu-item-url handles non lower case protocol"
      (string-intersperse '(
        "1A good gopher example\t\texample.com\t70"
        "hA good http example\tURL:htTP://example.com\tlocalhost\t70"
        ".\r\n")
        "\r\n")
      (parameterize ((server-hostname "localhost") (server-port 70))
        (let* ((urls '(
                 ("GoPher://example.com" . "A good gopher example")
                 ("htTP://example.com" . "A good http example")))
               (menu (map (lambda (x) (menu-item-url (cdr x) (car x)))
                          urls)))
          (menu-render menu) ) ) )


  (test "menu-item-url handles urls containing a ':' in path"
        (string-intersperse '(
          "hOld Example\tURL:https://example.com/http://old.example.com/\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (parameterize ((server-hostname "localhost") (server-port 70))
          (let ((username "Old Example")
                (url "https://example.com/http://old.example.com/"))
            (menu-render (list (menu-item-url username url) ) ) ) ) )


  (test "menu-item-url returns #f if protocol is unsupported"
        #f
        (parameterize ((server-hostname "localhost") (server-port 70))
          (menu-item-url "Something interesting" "fred://example.com") ) )


  (test "menu-item-url returns #f if URL isn't valid"
        #f
        (parameterize ((server-hostname "localhost") (server-port 70))
          (menu-item-url "Something interesting" "invalidURL") ) )


)

