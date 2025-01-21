;;; Tests for the index procedures

(test-group "index"


  (test "process-index returns #f and logs an error if file in 'index' doesn't exist"
        (list #f "[ERROR] selector: dir-a, index line: 1, error processing index\n")
        (let ((index "=> nonexistent.txt")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (error-logger-config
                           (config-logger (error-logger-config)
                                          port: port)))
            (list (process-index fixtures-dir "dir-a" index)
                  (get-output-string port) ) ) ) )


  (test "process-index returns #f and logs an error if an absolute link in 'index' is unsafe"
        (list #f "[ERROR] selector: dir-a, index line: 1, error processing index\n")
        (let ((index "=> /../run.scm An unsafe absolute link\n")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (error-logger-config
                           (config-logger (error-logger-config)
                                          port: port)))
            (list (process-index fixtures-dir "dir-a" index)
                  (get-output-string port) ) ) ) )


  (test "process-index returns #f and logs an error if a link to a directory doesn't have a trailing '/'"
        (list #f "[ERROR] selector: dir-b, index line: 1, error processing index\n")
        (let ((index "=> dir-ba This is actually a directory")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (error-logger-config
                           (config-logger (error-logger-config)
                                          port: port)))
            (list (process-index fixtures-dir "dir-b" index)
                  (get-output-string port) ) ) ) )


  (test "process-index returns #f and logs an error if a relative link in 'index' is unsafe"
        (list #f "[ERROR] selector: dir-a, index line: 1, error processing index\n")
        (let ((index "=> ../run.scm An unsafe relative link")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (error-logger-config
                           (config-logger (error-logger-config)
                                          port: port)))
            (list (process-index fixtures-dir "dir-a" index)
                  (get-output-string port) ) ) ) )


  (test "process-index returns #f and logs an error if a URL link protocol is unknown"
        (list #f "[ERROR] selector: dir-a, index line: 1, error processing index\n")
        (let ((index "=> fred://example.com")
              (port (open-output-string)))
          (parameterize ((log-level 0)
                         (error-logger-config
                           (config-logger (error-logger-config)
                                          port: port)))
            (list (process-index fixtures-dir "dir-a" index)
                  (get-output-string port) ) ) ) )

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
                       "")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index only recognizes links where => is at the beginning of the line"
        (string-intersperse '(
          "1A link with => starting at the beginning of the line\t\tlocalhost\t70"
          "i => / This isn't a link because => doesn't start at the beginning of the line\tdir-a\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> / A link with => starting at the beginning of the line"
                       " => / This isn't a link because => doesn't start at the beginning of the line")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index supports absolute links"
        (string-intersperse '(
          "1Back to the beginning\t\tlocalhost\t70"
          "1/dir-a\tdir-a\tlocalhost\t70"
          "0/a.txt\ta.txt\tlocalhost\t70"
          "1Lots of white space (will be removed)\tdir-a\tlocalhost\t70"
          "0Lots of white space (will be removed)\tb.txt\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> / Back to the beginning"
                       "=> /dir-a/"
                       "=> /a.txt"
                       "=>     /dir-a/    Lots of white space (will be removed)    "
                       "=>     /b.txt     Lots of white space (will be removed)    ")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


  (test "process-index supports relative links"
        (string-intersperse '(
          "1dir-ba\tdir-b/dir-ba\tlocalhost\t70"
          "1The ba directory\tdir-b/dir-ba\tlocalhost\t70"
          "1The bb directory\tdir-b/dir-bb\tlocalhost\t70"
          "0dir-ba/baa.txt\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
          "9dir-ba/bac.bin\tdir-b/dir-ba/bac.bin\tlocalhost\t70"
          "1Lots of white space (will be removed)\tdir-b/dir-ba\tlocalhost\t70"
          "0Lots of white space (will be removed)\tdir-b/dir-ba/baa.txt\tlocalhost\t70"
         ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> dir-ba/"
                       "=> dir-ba/ The ba directory"
                       "=> dir-bb/ The bb directory"
                       "=> dir-ba/baa.txt"
                       "=> dir-ba/bac.bin"
                       "=>     dir-ba/            Lots of white space (will be removed)    "
                       "=>     dir-ba/baa.txt     Lots of white space (will be removed)    ")
                       "\n")))

          (menu-render (process-index fixtures-dir "dir-b" index) ) ) )


  (test "process-index supports links to directories that don't exist as long as the link ends with '/'"
        (string-intersperse '(
          "1/unknown\tunknown\tlocalhost\t70"
          "1unknown\tdir-a/unknown\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index (string-intersperse '(
                       "=> /unknown/"
                       "=> unknown/")
                       "\n")))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )


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
          (menu-render (process-index fixtures-dir "dir-b" index) ) ) )


  (test "process-index supports URL links with a ':' in path"
        (string-intersperse '(
          "hOld Example\tURL:https://example.com/http://old.example.com/\tlocalhost\t70"
          ".\r\n")
          "\r\n")
        (let ((index "=> https://example.com/http://old.example.com/ Old Example"))
          (menu-render (process-index fixtures-dir "dir-a" index) ) ) )



)
