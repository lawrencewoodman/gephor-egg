;;; Miscellaneous definitions
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   selector->local-path trim-path-selector safe-path? safe-read-file
;;;
;;; Copyright (C) 2024-2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

;; Exported Definitions ------------------------------------------------------

;; Converts a selector string into a local-path string by prepending root-dir.
;; It also confirms that the path is safe.
;;
;; The selector must be a valid file path.  Whitespace and '/' characters
;; will be trimmed from both ends of the selector to ensure safe and
;; predicatable local path creation.  Whitespace is trimmed even though
;; it should be by the server because the removal of a leading or terminating
;; '/' character might leave whitespace.
;;
;; Returns:
;;   Ok if everything was ok
;;   Error if the path isn't safe
(: selector->local-path (string string --> *))
(define (selector->local-path root-dir selector)
  (let* ((root-dir (if (> (string-length root-dir) 1)
                       (string-chomp root-dir "/")
                       root-dir))
         (selector (trim-path-selector selector))
         (local-path (make-pathname root-dir selector)))
    (if (safe-path? root-dir local-path)
        (Ok local-path)
        (Error "path isn't safe" (list (cons 'local-path local-path) ) ) ) ) )


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
(: safe-path? (string string --> boolean))
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


;; Read the contents of a file
;;
;; Returns:
;;   Ok with the contents of the file
;;   Error if:
;;     The file is bigger than max-size
;;     The file isn't world readable
;;     The file path isn't safe
(: safe-read-file (integer string string --> *))
(define (safe-read-file max-size root-dir path)
  (if (world-readable? path)
      (if (safe-path? root-dir path)
          (unsafe-read-file max-size path)
          (Error "can't read file, file path isn't safe"
                 (list (cons 'file path))))
      (Error "can't read file, file path isn't world readable"
             (list (cons 'file path) ) ) ) )


;; Internal Definitions ------------------------------------------------------

;; A char set for trimming selectors
(define path-selector-trim-char-set
  (char-set-adjoin char-set:whitespace #\/) )


;; Similar to error but passes arguments after location to sprintf to form
;; error message
(define (error* location . args)
  (error location (apply sprintf args) ) )


;; Used by safe-read-file.  This function reads a file without checking if
;; the path is safe and world readable.
;;
;; Returns:
;;   Ok with the contents of the file
;;   Error if:
;;     The file is bigger than max-size
(: unsafe-read-file (integer string --> *))
(define (unsafe-read-file max-size path)
  (call-with-input-file path
                        (lambda (port)
                          ; This checks the size while reading rather than
                          ; before reading in case the file changes size
                          (let* ((contents (read-string max-size port))
                                 (more? (not (eof-object? (read-string 1 port)))))
                            (if (eof-object? contents)
                                (Ok "")
                                (if more?
                                    (Error "can't read file, file is too big"
                                           (list (cons 'file path)))
                                    (Ok contents)))))
                        #:binary) )
