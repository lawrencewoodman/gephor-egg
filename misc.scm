;;; Miscellaneous definitions
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   trim-path-selector safe-path?
;;;
;;; Copyright (C) 2024-2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

;; Exported Definitions ------------------------------------------------------

;; Trim beginning and end of selector to remove whitespace and
;; '/' characters
(define (trim-path-selector selector)
  (string-trim-both selector path-selector-trim-char-set) )


;; We want to prevent directory traversal attacks to ensure that path
;; can't reach the parent directory of root-dir  We include '\' in the
;; list of invalid characters because of an apparent bug according to
;; the Spiffy web server source code which says that this can be turned
;; into a '/' sometimes by Chicken. root-dir must be an absolute path.
;; NOTE: If chicken scheme starts supporting UTF-8 properly then we will
;; NOTE: need to worry about percent decoding which should be done before
;; NOTE: any other checks.
;; NOTE: This does not check if the path is world readable
;; TODO: Should we test for nul in a string as per Spiffy?
(define (safe-path? root-dir path)
  (when (not (absolute-pathname? root-dir))
        (error* 'safe-path? "root-dir must be an absolute directory: ~A" root-dir))
  (let* ((n-root-dir (normalize-pathname root-dir))
         (n-path (normalize-pathname path)))
    (let-values (((_ n-root-base n-root-elements) (decompose-directory n-root-dir))
                 ((_ n-path-base n-path-elements) (decompose-directory n-path)))
      (and (not (substring-index "./" path))
           (not (substring-index ".." path))
           (not (substring-index "\\" path))
           (equal? n-root-base n-path-base)
           (or (not n-root-elements)
               (and n-path-elements
                    (>= (length n-path-elements) (length n-root-elements))
                    (equal? n-root-elements
                           (take n-path-elements
                                 (length n-root-elements) ) ) ) ) ) ) ) )


;; If the file is bigger than max-size it will return #f and log an error.
;; If the file isn't world readable it will return #f and log an error.
;; If the file path isn't safe it will return #f and log an error.
;; Otherwise, it will return the contents of the file
(: safe-read-file (integer string string -> (or string false)))
(define (safe-read-file max-size root-dir path)
  (printf "path word readable? ~A, path: ~A~%" (world-readable? path) path)
  (if (world-readable? path)
      (if (safe-path? root-dir path)
          (unsafe-read-file max-size path)
          (begin
            (apply log-error
                   "file path isn't safe"
                   (cons 'file path)
                   (log-context))
            #f))
      (begin
        (apply log-error
               "file isn't world readable"
               (cons 'file path)
               (log-context))
        #f) ) )


;; Internal Definitions ------------------------------------------------------

;; A char set for trimming selectors
(define path-selector-trim-char-set
  (char-set-adjoin char-set:whitespace #\/) )


;; Similar to error but passes arguments after location to sprintf to form
;; error message
(define (error* location . args)
  (error location (apply sprintf args) ) )


;; Used by safe-read-file.  This function reads a file without checking if
;; the path is safe and world readable
(: unsafe-read-file (integer string -> (or string false)))
(define (unsafe-read-file max-size path)
  (call-with-input-file path
                        (lambda (port)
                          ; This checks the size while reading rather than
                          ; before reading in case the file changes size
                          (let* ((contents (read-string max-size port))
                                 (more? (not (eof-object? (read-string 1 port)))))
                            (if (eof-object? contents)
                                ""
                                (if more?
                                    (begin
                                      (apply log-error
                                             "file is too big to read"
                                             (cons 'file path)
                                             (cons 'max-size max-size)
                                             (log-context))
                                      #f)
                                    contents))))
                        #:binary) )
