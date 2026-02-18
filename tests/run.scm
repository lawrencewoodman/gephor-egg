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
        datatype
        logfmt-logger
        srfi-1)

;; Import notes -------------------------------------------------------------
;; srfi-1         - List procedures
;; datatype       - Variant records
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

;; Check that the symbol in log-entries matches the regex.  If it does it changes
;; the value to #t
(define (confirm-field-matches symbol regex log-entries)
  (alist-update symbol
               (irregex-match? regex (alist-ref symbol log-entries))
               log-entries) )


;; Test each exported component
(include-relative "result.scm")
(include-relative "router.scm")
(include-relative "menu.scm")
(include-relative "response.scm")
(include-relative "handlers.scm")
(include-relative "misc.scm")
(include-relative "server.scm")

(test-exit)
