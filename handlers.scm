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
    (and (safe-path? root-dir local-path)
         local-path) ) )
;; TODO: log-warning if path not safe


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
(define (serve-dir root-dir request)
  (let ((selector (request-selector request))
        (client-address (request-client-address request)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (and-let* ((local-path (selector->local-path root-dir selector)))
      (and (directory? local-path)
           (and-let* ((response (list-dir selector local-path)))
             (log-info "serve directory listing"
                       (cons 'handler 'serve-dir)
                       (cons 'directory local-path)
                       (cons 'selector selector)
                       (cons 'client-address client-address))
             (menu-render response) ) ) ) ) )


;; If the path formed by root-dir and request is a regular file and readable
;; return the file.
;; See selector->local-path for more information about selector requirements.
;; Returns #f if can't return a file.
(define (serve-file root-dir request)
  (let ((selector (request-selector request))
        (client-address (request-client-address request)))
    ;; local-path is formed here rather than being passed in to ensure that it
    ;; is formed safely
    (and-let* ((local-path (selector->local-path root-dir selector)))
      (and (regular-file? local-path)
           (and-let* ((response (read-file local-path)))
             (log-info "serve file"
                       (cons 'handler 'serve-file)
                       (cons 'file local-path)
                       (cons 'selector selector)
                       (cons 'client-address client-address))
             response) ) ) ) )


;; Serve an html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; Returns #f if failure
;; TODO: rename
(define (serve-url request)
  (let ((selector (request-selector request))
        (client-address (request-client-address request)))
    (and (substring=? (request-selector request) "URL:")
         (let* ((url (substring (request-selector request) 4)))
           (log-info "serve URL as HTML page"
                     (cons 'handler 'serve-url)
                     (cons 'url url)
                     (cons 'selector selector)
                     (cons 'client-address client-address))
           (string-translate* url-html-template (list (cons "@URL" url) ) ) ) ) ) )

;; TODO: add a connection id to all log key value pairs to join all messages for a connection


;; Internal Definitions ------------------------------------------------------

;; Does the file have word readable permissions?
(define (world-readable? filename)
  (= perm/iroth (bitwise-and (file-permissions filename) perm/iroth)))


;; Parameter: max-file-size controls the maximum size file
;; that can be read, anything bigger than this returns an Error Result.
;; Returns #f if not world readable or file is too big
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
                                          (log-warning "file is too big to read"
                                                       (cons 'file path)
                                                       (cons 'max-file-size (max-file-size)))
                                          #f)
                                        contents))))
                            #:binary)
      (begin
        (log-warning "file isn't world readable"
                     (cons 'file path)
                     (cons 'function "read-file"))
        #f) ) )
;; TODO: Document what a warning is as it may not be obvious


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
            (else #f))))

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

  (and (world-readable? local-path)
       (let* ((filenames (directory local-path))
              (entries (sort-dir-entries (filter-map make-dir-entry filenames)))
              (menu (do ((entries entries (cdr entries))
                         (result '() (let* ((item (entry->menu-item (car entries))))
                                       (if item (cons item result) result))))
                        ((null? entries) result))))
         (reverse menu) ) ) )



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

