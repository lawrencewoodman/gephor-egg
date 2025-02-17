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

;; TODO: Add test
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
;; TODO: Add test
(define (safe-path? root-dir path)
  (let ((n-root-dir (normalize-pathname root-dir))
        (n-path (normalize-pathname path)))
    (and (absolute-pathname? root-dir)
         (not (substring-index "./" path))
         (not (substring-index ".." path))
         (not (substring-index "\\" path))
         (>= (string-length n-path) (string-length n-root-dir))
         (substring=? n-root-dir n-path) ) ) )


;; Internal Definitions ------------------------------------------------------

;; A char set for trimming selectors
(define path-selector-trim-char-set
  (char-set-adjoin char-set:whitespace #\/) )

;; Similar to error but passes arguments after location to sprintf to form
;; error message
(define (error* location . args)
  (error location (apply sprintf args) ) )

