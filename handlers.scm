;;; The route handlers
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   selector->local-path serve-dir serve-file serve-path serve-url
;;;
;;; Copyright (C) 2024-2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;
;;; NOTE: Any handler should log-info if it is the correct handler
;;; TODO: Check if NOTE about log-info still makes sense


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
;; Returns #f on failure.
(define (selector->local-path root-dir selector)
  (let* ((root-dir (if (> (string-length root-dir) 1)
                       (string-chomp root-dir "/")
                       root-dir))
         (selector (trim-path-selector selector))
         (local-path (make-pathname root-dir selector)))
    (if (safe-path? root-dir local-path)
        local-path
        (begin
          (apply log-warning
                 "path isn't safe"
                 (cons 'path local-path)
                 (log-context))
          #f) ) ) )


;; Tries the following handlers in turn until one returns non-false or the
;; last one fails:
;;   serve-file serve-dir
;; See the documentation for each handler for more information.
;; Returns the value of the last handler tried.
(define (serve-path root-dir request)
  (any (lambda (h) (h root-dir request))
       (list serve-file serve-dir) ) )


;; If the path formed by root-dir and request is a diretory return a menu
;; listing the files and directories in the directory.
;; See selector->local-path for more information about selector requirements.
;; Returns #f if can't list a directory.
(: serve-dir (string * -> (or string false)))
(define (serve-dir root-dir request)
  (let ((selector (request-selector request)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (and-let* ((local-path (selector->local-path root-dir selector)))
      (and (directory? local-path)
           (and-let* ((response (list-dir selector local-path)))
             (apply log-info
                    "serve directory listing"
                    (cons 'handler 'serve-dir)
                    (cons 'directory local-path)
                    (log-context))
             (menu-render response) ) ) ) ) )


;; If the path formed by root-dir and request is a regular file and readable
;; return the file.
;; See selector->local-path for more information about selector requirements.
;; Returns #f if can't return a file.
(: serve-file (string * -> (or string false)))
(define (serve-file root-dir request)
  (let ((selector (request-selector request)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (and-let* ((local-path (selector->local-path root-dir selector)))
      (and (regular-file? local-path)
           (and-let* ((response (read-file local-path)))
             (apply log-info
                    "serve file"
                    (cons 'handler 'serve-file)
                    (cons 'file local-path)
                    (log-context))
             response) ) ) ) )


;; Serve an html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; Returns #f if failure
;; TODO: rename
(: serve-url (* -> (or string false)))
(define (serve-url request)
  (let ((selector (request-selector request)))
    (and (substring=? (request-selector request) "URL:")
         (let* ((url (substring (request-selector request) 4)))
           (apply log-info
                  "serve URL as HTML page"
                  (cons 'handler 'serve-url)
                  (cons 'url url)
                  (log-context))
           (string-translate* url-html-template (list (cons "@URL" url) ) ) ) ) ) )


;; Parameter: max-file-size controls the maximum size file
;; that can be read, anything bigger than this will return #f
;; and log an error  If the file isn't world readable #f will
;; be returned and it will log an error.
;; TODO: Now this is exported it should probably be renamed to reduce
;; TODO: the chance of name clashes
;; TODO: Place this in another file?
(: read-file (string -> (or string false)))
(define (read-file path)
  (if (world-readable? path)
      (call-with-input-file path
                            (lambda (port)
                              (let* ((contents (read-string (max-file-size) port))
                                     (more? (not (eof-object? (read-string 1 port)))))
                                (if (eof-object? contents)
                                    ""
                                    (if more?
                                        (begin
                                          (apply log-error
                                                 "file is too big to read"
                                                 (cons 'file path)
                                                 (cons 'max-file-size (max-file-size))
                                                 (log-context))
                                          #f)
                                        contents))))
                            #:binary)
      (begin
        (apply log-error
               "file isn't world readable"
               (cons 'file path)
               (log-context))
        #f) ) )
;; TODO: Document what a warning is as it may not be obvious


;; Internal Definitions ------------------------------------------------------

;; Does the file have word readable permissions?
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


;; TODO: Move this, export? and rename?
;; TODO: Make sure paths and selectors are safe
;; Returns #f if not world readable, otherwise a list of menu
;; items representing the files in the directory
;; If not world readable it logs an error message
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

  ;; TODO: Truncate usernames for filenames that are > 69 characters
  (define (entry->menu-item entry)
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

  (if (world-readable? local-path)
      (let* ((filenames (directory local-path))
             (entries (sort-dir-entries (filter-map make-dir-entry filenames)))
             (menu (do ((entries entries (cdr entries))
                        (result '() (let ((item (entry->menu-item (car entries))))
                                      (if item
                                          (cons item result)
                                           result))))
                        ((null? entries) result))))
        ;; TODO: If resulting menu is empty should return #f and log an error
        (reverse menu))
      (begin
        (apply log-error
               "can't list dir, path isn't world readable"
               (cons 'local-path local-path)
               (log-context))
        #f) ) )



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

