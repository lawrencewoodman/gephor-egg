;;; Tests for the index procedures

(test-group "index"


  (test "process-index returns Error if file in 'index' doesn't exist"
        (Error (list "error processing index"
                     (sprintf "local-path: ~A, file doesn't exist"
                              (make-pathname (list fixtures-dir
                                                   "dir-a")
                                             "nonexistent.txt"))))
        (let ((index "=> nonexistent.txt"))
          (process-index fixtures-dir "dir-a" index) ) )


  (test "process-index returns Error if an absolute link in 'index' is unsafe"
        (Error (list "error processing index"
                     (sprintf "path: /../run.scm, full-path: ~A/../run.scm, isn't safe"
                              fixtures-dir)))
        (let ((index "=> /../run.scm An unsafe absolute link\n"))
          (process-index fixtures-dir "dir-a" index) ) )


  (test "process-index returns Error if a relative link in 'index' is unsafe"
        (Error (list "error processing index"
                     (sprintf "path: ../run.scm, full-path: ~A/dir-a/../run.scm, isn't safe"
                              fixtures-dir)))
        (let ((index "=> ../run.scm An unsafe relative link"))
          (process-index fixtures-dir "dir-a" index) ) )


  (test "process-index returns Error if a URL link protocol is unknown"
        (Error (list "error processing index"
                     "url: fred://example.com, unsupported protocol: fred"))
        (let ((index "=> fred://example.com"))
          (process-index fixtures-dir "dir-a" index) ) )


  (test "process-index removes blank lines at top and bottom of index"
        (string-intersperse '(
          "iThere are blank lines above and below this line that should be stripped\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       ""
                       ""
                       "There are blank lines above and below this line that should be stripped"
                       ""
                       ""))))
          (cases Result
                 (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (Error (e) e) ) ) )



  (test "process-index supports absolute links"
        (string-intersperse '(
          "1Back to the beginning\t\tlocalhost\t70"
          "1/dir-a\tdir-a\tlocalhost\t70"
          "0/a.txt\ta.txt\tlocalhost\t70"
          "0Lots of white space (will be removed)\tb.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> / Back to the beginning"
                       "=> /dir-a/"
                       "=> /a.txt"
                       "=>     /b.txt     Lots of white space (will be removed)    ")
                       "\n")))
          (cases Result
                 (process-index fixtures-dir "dir-a" index)
                 (Ok (v) (menu-render v))
                 (Error (e) e) ) ) )


  (test "process-index supports relative links"
        (string-intersperse '(
          "1dir-ba\tdir-b/dir-ba\tlocalhost\t70"
          "1The ba directory\tdir-b/dir-ba\tlocalhost\t70"
          "1The bb directory\tdir-b/dir-bb\tlocalhost\t70"
          "0dir-ba/baa.txt\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
          "9dir-ba/bac.bin\tdir-b/dir-ba/bac.bin\tlocalhost\t70"
          "0Lots of white space (will be removed)\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
         ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> dir-ba/"
                       "=> dir-ba/ The ba directory"
                       "=> dir-bb/ The bb directory"
                       "=> dir-ba/baa.txt"
                       "=> dir-ba/bac.bin"
                       "=> dir-ba/baa.txt     Lots of white space (will be removed)    ")
                       "\n")))

          (cases Result
                 (process-index fixtures-dir "dir-b" index)
                 (Ok (v) (menu-render v))
                 (Error (e) e) ) ) )


  (test "process-index supports URL links"
        (string-intersperse '(
          "hhttp://example.com\tURL:http://example.com\tlocalhost\t70"
          "hhttp://example.com/fred\tURL:http://example.com/fred\tlocalhost\t70"
          "hhttp://example.com/fred/\tURL:http://example.com/fred/\tlocalhost\t70"
          "hFred's things\tURL:http://example.com/fred\tlocalhost\t70"
          "hLots of white space (will be removed)\tURL:http://example.com/fred\tlocalhost\t70"
        ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> http://example.com"
                       "=> http://example.com/fred"
                       "=> http://example.com/fred/"
                       "=> http://example.com/fred Fred's things"
                       "=> http://example.com/fred      Lots of white space (will be removed)   ")
                       "\n")))
          (cases Result

                 (process-index fixtures-dir "dir-b" index)
                 (Ok (v) (menu-render v))
                 (Error (e) e) ) ) )



)
