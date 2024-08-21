;;; Index file definitions to process NEX style index files
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported: process-index
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------


;; Process a NEX style index file.  This describes a menu and returns a list
;; of menu items.
;; TODO: Rename index to something else
;; TODO: rename?
(define (process-index root-dir selector nex-index)

  (define (dir-item path username)
    (let ((item-selector (if (absolute-pathname? path)
                             (trim-path-selector path)
                             (make-pathname selector (string-chomp path "/")))))
      (menu-item 'menu username item-selector (server-hostname) (server-port) ) ) )

  (define (file-item path username)
    (define (make-item full-path item-selector)
      (cond
        ((unsafe-path? root-dir full-path)
           (Error-fmt "path: ~A, full-path: ~A, isn't safe" path full-path))
        ((directory? full-path)
           (Error-fmt "path: ~A, full-path: ~A, is a directory but link missing trailing '/'"
                      path full-path))
        (else
           (menu-item-file full-path username item-selector) ) ) )

    (if (absolute-pathname? path)
        (let ((full-path (make-pathname root-dir (trim-path-selector path))))
          (make-item full-path (trim-path-selector path)))
        (let ((full-path (make-pathname (list root-dir selector)
                                        (trim-path-selector path)))
              (item-selector (make-pathname selector path)))
          (make-item full-path item-selector) ) ) )

  (define (is-dir? path)
    (substring-index "/" path (sub1 (string-length path))))

  (define (is-url? path)
    (let ((url-match (irregex-match url-regex path)))
      (irregex-match-data? url-match)))

  (define (parse-line line)
    (let ((link-match (irregex-search index-link-split-regex line)))
      (if (irregex-match-data? link-match)
          (let* ((path (irregex-match-substring link-match 1))
                 (maybe-username (irregex-match-substring link-match 2))
                 (username (if (string=? maybe-username "")
                               path
                               maybe-username))
                 (chomped-username (if (string=? maybe-username "")
                                       (string-chomp path "/")
                                       maybe-username)))
            (cond
              ((is-url? path)
                (menu-item-url username path))
              ((is-dir? path)
                (dir-item path chomped-username))
              (else
                (file-item path username))))
          ;; Current selector is used for info itemtype so that if type
          ;; not supported by client but still displayed then it
          ;; will just link to the page that it is being displayed on
          (menu-item 'info line selector (server-hostname) (server-port) ) ) ) )

  (let* ((lines (string-split (string-trim-both nex-index char-set:whitespace) "\n" #t))
         (parsed-lines
           (do ((lines lines (cdr lines))
                (result '() (let ((item (parse-line (car lines))))
                              (cases Result item
                                (Ok (v) (cons v result))
                                (Error (e) (Error-wrap item "error processing index"))))))
                ((or (null? lines) (Error? result)) result))))
    (if (Error? parsed-lines)
        parsed-lines
        (Ok (reverse parsed-lines) ) ) ) )


;; Internal Definitions ------------------------------------------------------

;; Regular expression to split a => style link
;; TODO: Should => start at beginning of line?
(define index-link-split-regex (string->irregex "^[ ]*=>[ ]+([^ ]+)[ ]*(.*)$"))

;; Regular expression to identify a URL in a => style link
(define url-regex (string->irregex "^.*:\/\/[^:/]+(:[0-9]*)?.*$"))

