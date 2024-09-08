;;; The route handlers
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported: serve-url serve-path
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

;; Serve an html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; Returns #f if failure
;; TODO: rename
(define (serve-url request)
  (and (substring=? (request-selector request) "URL:")
       (let* ((url (substring (request-selector request) 4)))
         (log-handler-info "serve-url" request "serving url: ~A" url)
         (Ok (string-translate* url-html-template (list (cons "@URL" url) ) ) ) ) ) )


;; The selector must be a valid file path.  Whitespace and '/' characters
;; will be trimmed from both ends of the selector to ensure safe and
;; predicatable local path creation.  Whitespace is trimmed even though
;; it should be by the server because the removal of a leading or terminating
;; '/' character might leave whitespace.
;; TODO: Perhaps move the above advice elsewhere or handle same as Phricken
(define (serve-path request root-dir)
  (any (lambda (h) (h root-dir request))
       serve-path-handlers) )



;; Internal Definitions ------------------------------------------------------

;; Does the file have word readable permissions?
(define (world-readable? filename)
  (= perm/iroth (bitwise-and (file-permissions filename) perm/iroth)))


(define (log-handler-info handler-name request . args)
  (apply log-info (conc "client address: ~A, selector: ~A, handler: ~A, " handler-name (car args))
                  (request-client-address request) (request-selector request)
                  (cdr args) ) )


(define (log-handler-warning handler-name request . args)
  (apply log-warning (conc "client address: ~A, selector: ~A, handler: ~A, " handler-name (car args))
                     (request-client-address request) (request-selector request)
                     (cdr args)))



;; Parameter: max-file-size controls the maximum size file
;; that can be read, anything bigger than this returns an Error Result.
;; Returns #f if not world readable and an exception if file is too big
;; other exceptions raised as well
(define (read-file path)
  (and (world-readable? path)
       (call-with-input-file path
                             (lambda (port)
                               (let* ((contents (read-string (max-file-size) port))
                                      (more? (not (eof-object? (read-string 1 port)))))
                                 (if (eof-object? contents)
                                     ""
                                     (if more?
                                         (error* 'read-file "file: ~A, is greater than ~A bytes"
                                                     path
                                                     (max-file-size))
                                         contents))))
                             #:binary) ) )


;; Converts a selector string into a local-path string by prepending root-dir.
;; It also confirms that the path is safe, that it exists and is world
;; readable.  Returns #f on failure.
(define (selector->local-path root-dir selector)
  (let* ((root-dir (if (> (string-length root-dir) 1)
                       (string-chomp root-dir "/")
                       root-dir))
         (selector (trim-path-selector selector))
         (local-path (make-pathname root-dir selector)))
    (and (safe-path? root-dir local-path)
         (file-exists? local-path)
         local-path) ) )


(define (serve-index root-dir request)
  ;; TODO: Do we want to use 'index' as filename?
  (and-let* ((local-path (selector->local-path root-dir (request-selector request))))
    (and (directory? local-path)
         (let ((index-path (make-pathname local-path "index")))
           (and (file-exists? index-path)
                (and-let* ((nex-index (read-file index-path)))
                  (let ((response (process-index root-dir (request-selector request) nex-index)))
                    (cases Result response
                      (Ok (v) (Ok (menu-render v)))
                      (Error (e) (Error-wrap response "local-path: ~A, error serving index"
                                             local-path) ) ) ) ) ) ) ) ) )


(define (serve-file root-dir request)
  ;; TODO: allow or don't allow gophermap to be downloaded?
  (and-let* ((local-path (selector->local-path root-dir (request-selector request))))
    (and (regular-file? local-path)
         (begin
           (and-let* ((response (read-file local-path)))
             (log-handler-info "serve-file" request "request file: ~A" local-path)
             (Ok response) ) ) ) ) )


(define (serve-dir root-dir request)
  (and-let* ((local-path (selector->local-path root-dir (request-selector request))))
    (and (directory? local-path)
         (begin
           (log-handler-info "serve-dir" request "list directory: ~A" local-path)
           (let* ((response (list-dir (request-selector request) local-path)))
             (cases Result response
               (Ok (v) (Ok (menu-render v)))
               (Error (e) (Error-wrap response "local-path: ~A, error serving directory"
                                      local-path) ) ) ) ) ) ) )


;; List of handlers that serve-path will try
(define serve-path-handlers (list serve-index serve-file serve-dir))


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
;; TODO: Should this return false if failed?
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

  (if (world-readable? local-path)
      (let* ((filenames (directory local-path))
             (entries (sort-dir-entries (filter-map make-dir-entry filenames)))
             (menu (do ((entries entries (cdr entries))
                        (result '() (let* ((item (entry->menu-item (car entries))))
                                      (cases Result item
                                        (Ok (v) (cons v result))
                                        (Error (e) (Error-wrap item "error listing directory"))))))
                       ((or (null? entries) (Error? result)) result))))
        (if (Error? menu)
            menu
            (Ok (reverse menu))))
      (Error-fmt "local-path: ~A, isn't world readable" local-path) ) )



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

