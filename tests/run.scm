(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken condition)
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
        datatype
        simple-logger)


(load-relative "../gephor.scm")
(import gephor)

(define dummy)


;; TODO: Test log output
(log-level 100)


;; The path of the fixtures directory
(define fixtures-dir
  (let loop ((dirs (list (current-directory) (make-pathname (current-directory) "tests"))))
    (if (null? dirs) (error "can't find fixtures directory"))
    (let ((try-path (make-pathname (car dirs) "fixtures")))
      (if (and (file-exists? try-path) (directory? try-path))
          try-path
          (loop (cdr dirs))))))


;; Test each exported component
(include-relative "result.scm")
(include-relative "router.scm")
(include-relative "menu.scm")
(include-relative "handlers.scm")
(include-relative "server.scm")

(test-exit) 
