(import scheme
        test
        (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken format)
        (chicken file)
        (chicken file posix)
        (chicken io)
        (chicken irregex)
        (chicken load)
        (chicken pathname)
        (chicken process-context)
        (chicken string)
        (chicken tcp)
        (chicken time)
        logfmt-logger
        srfi-1)

;; Import notes -------------------------------------------------------------
;; srfi-1         - List procedures
;; logfmt-logger  - Logger using logfmt


(load-relative "../gephor.scm")
(import gephor)

(define dummy)

(log-level 100)


;; The path of the fixtures directory
(define fixtures-dir
  (let loop ((dirs (list (current-directory) (make-pathname (current-directory) "tests"))))
    (if (null? dirs) (error "can't find fixtures directory"))
    (let ((try-path (make-pathname (car dirs) "fixtures")))
      (if (and (file-exists? try-path) (directory? try-path))
          try-path
          (loop (cdr dirs))))))


;; Check log timestamp (ts) field is in the expected ISO 8601 format
;; Returns the log entries with ts=#t if timestamp is valid
(define (confirm-log-entries-valid-timestamp entry)
  (irregex-replace/all "ts=\\d\\d\\d\\d-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d[+-]\\d\\d\\d\\d "
                       entry
                       "ts=#t ") )


;; Run expressions and return list with two values:
;;   The return value of the last expression in body
;;   Any entries logged which are at log level or above.
;;   The log entries are run through confirm-log-entries-valid-timestamp first
(define-syntax run/get-log
    (syntax-rules ()
      ((run/get-log level expr expr* ...)
        (parameterize ((log-level level)
                       (log-port (open-output-string)))
          (let* ((ret (begin expr expr* ...))
                 (log (confirm-log-entries-valid-timestamp
                        (get-output-string (log-port)))))
            (close-output-port (log-port))
            (list ret log) ) ) ) ) )


;; Replace exception message using regex and replace-string
(define (confirm-exn-msg-regex exn regex replace-string)
  (irregex-replace/all regex
                       (get-condition-property exn 'exn 'message)
                       replace-string) )


;; Test each exported component
(include-relative "router.scm")
(include-relative "menu.scm")
(include-relative "response.scm")
(include-relative "handlers.scm")
(include-relative "misc.scm")
(include-relative "server.scm")

(test-exit)
