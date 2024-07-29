(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken format)
        (chicken file)
        (chicken file posix)
        (chicken load)
        (chicken io)
        (chicken pathname)
        (chicken process-context)
        (chicken port)
        (chicken string)
        (chicken tcp)
        simple-logger)


(load-relative "../gephor.scm")
(import gephor)

(define dummy)


;; TODO: Test log output
(log-level 100)


;; Test each exported component
(include-relative "router.scm")
(include-relative "menu.scm")
(include-relative "handlers.scm")
(include-relative "server.scm")

