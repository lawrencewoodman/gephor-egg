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

)

