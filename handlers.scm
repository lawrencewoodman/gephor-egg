;;; The route handlers
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   serve-dir serve-file serve-path serve-url
;;;
;;; Copyright (C) 2024-2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

;; Tries the following handlers in turn until one returns non-false or the
;; last one fails:
;;   serve-file serve-dir
;; See the documentation for each handler for more information.
;; Returns the value of the last handler tried, this will be:
;;   Ok if everything was ok
;;   Not-Applicable if neither of the handlers can handle the request
;;   Error if there was a problem
(: serve-path (string * -> *))
(define (serve-path root-dir request)
  (let ((response (serve-dir root-dir request)))
    (cases Result response
      (Ok () response)
      (Not-Applicable () (serve-file root-dir request))
      (Error () response) ) ) )


;; If the path formed by root-dir and request is a directory return a menu
;; listing the files and directories in the directory.
;; See selector->local-path for more information about selector requirements.
;; Returns the value of the last handler tried, this will be:
;;   Ok if everything was ok this will contain a listing of the directory
;;   Not-Applicable if the path isn't a directory
;;   Error if there was a problem
(: serve-dir (string * -> *))
(define (serve-dir root-dir request)
  (let* ((selector (request-selector request))
         (rlocal-path (selector->local-path root-dir selector)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (cases Result rlocal-path
      (Ok (local-path) (if (directory? local-path)
                           (let ((response (list-dir selector local-path)))
                             (cases Result response
                               (Ok (v) (Ok (menu-render v)))
                               (Error () response)))
                           (Not-Applicable #t)))
      (Error (msg log-entries) rlocal-path) ) ) )


;; If the path formed by root-dir and request is a regular file and readable
;; return the file.
;; See selector->local-path for more information about selector requirements.
;; Returns the value of the last handler tried, this will be:
;;   Ok if everything was ok this will contain the contents of the file
;;   Not-Applicable if the path isn't a regular file
;;   Error if there was a problem
(: serve-file (string * -> *))
(define (serve-file root-dir request)
  (let* ((selector (request-selector request))
         (rlocal-path (selector->local-path root-dir selector)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (cases Result rlocal-path
      (Ok (local-path) (if (regular-file? local-path)
                           (safe-read-file (max-response-size) root-dir local-path)
                           (Not-Applicable #t)))
      (Error (msg log-entries) rlocal-path) ) ) )


;; Serve an html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; Also:
;;   resources/h_type.txt
;; Returns #f if failure
(: serve-url (* -> *))
(define (serve-url request)
  (let ((selector (request-selector request)))
    (if (substring=? (request-selector request) "URL:")
        (let* ((url (substring (request-selector request) 4)))
          (Ok (string-translate* url-html-template (list (cons "@URL" url)))))
        (Not-Applicable #t) ) ) )


;; Internal Definitions ------------------------------------------------------

;; A directory entry used by list-dir
(define-type dir-entry (list string boolean string))

;; Does the file have world readable permissions?
(define (world-readable? filename)
  (= perm/iroth (bitwise-and (file-permissions filename) perm/iroth)))


;; Sort files so that directories come before regular files and then
;; sort in alphabetical order of the filename
(define (sort-dir-entries entries)
  (sort entries
        (lambda (a b)
          (let ((a-is-dir (second a))
                (b-is-dir (second b)))
            (cond
              ((and a-is-dir (not b-is-dir)) #t)
              ((and b-is-dir (not a-is-dir)) #f)
              (else
                (let ((a-filename (car a))
                      (b-filename (car b)))
                  (string<? a-filename b-filename))))))))


;; TODO: Make sure paths and selectors are safe
;; Returns the directory as a list of menu items
;; items representing the files in the directory
;; If not world readable it raises an error
(: list-dir (string string --> (list-of menu-item)))
(define (list-dir selector local-path)
  ;; An entry consists of a list (filename is-dir? selector)
  ;; This trims the selector to create a correct menu entry
  ;; Returns #f if not a valid file
  (define (make-dir-entry filename)
    (let ((full-local-filename (make-pathname local-path filename))
          (selector (make-pathname (trim-path-selector selector) filename)))
      (cond ((directory? full-local-filename)
              (list filename #t selector))
            ((regular-file? full-local-filename)
              (list filename #f selector))
            (else #f) ) ) )

  (define (make-menu entries)
    (do ((entries entries (cdr entries))
         (result '() (let ((item (dir-entry->menu-item local-path
                                                       (car entries))))
                       (if item
                           (cons item result)
                           result))))
        ((null? entries) result) ) )

  (if (world-readable? local-path)
      (let* ((filenames (directory local-path))
             (entries (sort-dir-entries (filter-map make-dir-entry filenames)))
             (menu (make-menu entries)))
        ;; TODO: If resulting menu is empty should return #f and log an error
        (Ok (reverse menu)))
      (Error "can't list dir, path isn't world readable"
             (list (cons 'local-path local-path) ) ) ) )


;; Return a menu item from a directory entry in list-dir
(: dir-entry->menu-item (string dir-entry --> menu-item))
(define (dir-entry->menu-item local-path entry)
  (let ((filename (first entry))
         (is-dir? (second entry))
         (selector (third entry)))
    (if is-dir?
        (menu-item 'menu
                   filename
                   selector
                   (server-hostname)
                   (server-port))
        (menu-item-file (make-pathname local-path filename)
                        filename
                        selector) ) ) )


;; The HTML template used by serve-url
(define url-html-template #<<END
<HTML>
  <HEAD>
    <META HTTP-EQUIV="refresh" content="2;URL=@URL">
  </HEAD>
  <BODY>
    You are following a link from gopher to a web site.  You will be
    automatically taken to the web site shortly.  If you do not get sent
    there, please click
    <A HREF="@URL">here</A> to go to the web site.
    <P>
    The URL linked is:
    <P>
    <A HREF="@URL">@URL</A>
    <P>
    Thanks for using gopher!
  </BODY>
</HTML>
END
)

