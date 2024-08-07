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
;; TODO: rename
(define (serve-url context request)
  (if (not (substring=? (request-selector request) "URL:"))
      (error* 'serve-url "invalid selector: ~A" (request-selector request))
      (let* ((url (substring (request-selector request) 4)))
        (log-info "client address: ~A, selector: ~A, handler: serve-url, url: ~A"
                  (request-client-address request)
                  (request-selector request)
                  url)
        (Ok (string-translate* url-html-template (list (cons "@URL" url) ) ) ) ) ) )


;; TODO: Move this note about selectors not being file paths as not relevant
;; TODO: to serve-path and in fact should specify that it must be a valid
;; TODO: POSIX? path.
;; NOTE: When dealing with paths we must remember that a selector has no
;; NOTE: notion of a file path.  It is purely up to the server to decide
;; NOTE: how to understand it.
;;
;; TODO: Test how this handles an empty selector, the same as "/"?
;; TODO: How should this handle selectors with directories ending with and without "/"
(define (serve-path context request root-dir)

  (define (log-info* . args)
    (apply log-info (conc "client address: ~A, selector: ~A, handler: serve-path, " (car args))
                    (request-client-address request) (request-selector request)
                    (cdr args)))
  (define (log-warning* . args)
    (apply log-warning (conc "client address: ~A, selector: ~A, handler: serve-path, " (car args))
                       (request-client-address request) (request-selector request)
                       (cdr args)))


  ;; We want to prevent root traversal attacks and include '\' in the
  ;; list of invalid characters because of an apparent bug according to
  ;; spiffy source code which says that this can be turned into a '/'
  ;; sometimes by Chicken
  (define (invalid-root-dir? dir)
    (or (not (absolute-pathname? root-dir))
        (substring-index "/" root-dir (sub1 (string-length root-dir)))
        (substring-index "\\" root-dir)
        (substring-index ".." root-dir) ) )

  ;; TODO: Rename local-path ?
  ;; TODO: Check selector and path are safe
  ;; TODO: Find a better way of reducing safety check of root-dir and local-path
  (let ((local-path (make-pathname root-dir (request-selector request))))
    (cond ((invalid-root-dir? root-dir)
            (error* 'serve-path "root-dir isn't valid: ~A" root-dir))
          ((unsafe-pathname? local-path)   ;; TODO: Is this right for NEX style now?
            (log-warning* "selector isn't safe")
            ;; TODO: Should each of these make-rendered-error-menu calls
            ;; TODO: return an Error with message to give to client
            (Ok (make-rendered-error-menu context request "invalid selector")))
          ((not (file-exists? local-path))
            (log-warning* "local path doesn't exist: ~A" local-path)
            (Ok (make-rendered-error-menu context request "path not found")))
          ((not (world-readable? local-path))
            (log-warning* "local path isn't world readable: ~A" local-path)
            (Ok (make-rendered-error-menu context request "path not found")))
          ;; TODO: allow or don't allow gophermap to be downloaded?
          ((regular-file? local-path)
            (log-info* "request file: ~A" local-path)
            ;; TODO: Test error from this if fails
            (let ((response (read-file local-path)))
              (cases Result response
                (Ok (v) response)
                (Error (e) (Error-wrap response "local-path: ~A, error serving file"
                                       local-path)))) )
          ((directory? local-path)
            (log-info* "list directory: ~A" local-path)
            (let ((response
                    (if (file-exists? (make-pathname local-path "index"))
                        ;; TODO: Check index is world readable and otherwise suitable
                        ;; TODO: Do we want to use 'index' as filename?
                        (let ((nex-index (read-file (make-pathname local-path "index"))))
                          (cases Result nex-index
                            (Ok (v) (process-nex-index context
                                                       (request-selector request)
                                                       root-dir
                                                       local-path
                                                       v))
                            (Error (e) nex-index)))
                        (list-dir context (request-selector request) local-path))))
              (cases Result response
                (Ok (v) (Ok (menu-render v)))
                (Error (e) (Error-wrap response "local-path: ~A, error serving directory"
                                       local-path)))))
          (else
            (Error-fmt "unsupported file type for path: ~A" local-path) ) ) ) )

;; TODO: Test list-dir above


;; Internal Definitions ------------------------------------------------------

;; Regular expression to split a => style link
(define index-link-split-regex (string->irregex "^[ ]*=>[ ]+([^ ]+)[ ]*(.*)$"))

;; Regular expression to identify a URL in a => style link
(define url-regex (string->irregex "^.*:\/\/[^:/]+(:[0-9]*)?.*$"))


;; Does the file have word readable permissions?
(define (world-readable? filename)
  (= perm/iroth (bitwise-and (file-permissions filename) perm/iroth)))


;; Parameter: max-file-size controls the maximum size file
;; that can be read, anything bigger than this returns an Error Result.
(define (read-file path)
  (handle-exceptions ex
    (Error-ex ex "path: ~A, error reading file" path)
    (call-with-input-file path
                          (lambda (port)
                            (let* ((contents (read-string (max-file-size) port))
                                   (more? (not (eof-object? (read-string 1 port)))))
                              (if (eof-object? contents)
                                  (Ok "")
                                  (if more?
                                      (Error-fmt "file: ~A, is greater than ~A bytes"
                                                  path
                                                  (max-file-size))
                                      (Ok contents)))))
                      #:binary) ) )


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


;; Detect pathnames that are unsafe because they could lead to moving beyond
;; the intended folders or otherwise.  Backslashes are also detected
;; because of a warning about them in the Spiffy web server source code.
;; TODO: Should we text for nul in a string as per Spiffy?
;; TODO: Look also at pygopherd isrequestsecure function
(define (unsafe-pathname? pathname)
  (or (substring-index "./" pathname)
      (substring-index ".." pathname)
      (substring-index "\\" pathname)))


;; TODO: Move this, export? and rename?
;; TODO: Make sure paths and selectors are safe
;; TODO: Should this check if path is world-readable rather than calling proc?
(define (list-dir context selector local-path)
  ;; Returns #f if not a valid file
  ;; An entry consists of a list (filename is-dir? selector)
  (define (make-dir-entry filename)
    (let ((full-local-filename (make-pathname local-path filename))
          (selector (sprintf "~A~A~A"
                             selector
                             (if (string=? selector "") "" "/")
                             filename)))
      (cond ((directory? full-local-filename)
              (list filename #t selector))
            ((regular-file? full-local-filename)
              (list filename #f selector))
            (else #f))))

  (let* ((filenames (directory local-path))
         (hostname (context-hostname context))
         (port (context-port context))
         (entries (sort-dir-entries (filter-map make-dir-entry filenames))))
    (let loop ((entries entries) (res '{}))
      (if (null? entries)
          ;; TODO: Is there a quicker way of doing this rather than using reverse?
          (Ok (reverse res))
          (let ((entry (car entries)))
            (let ((item
                    (let* ((entry (car entries))
                           (filename (first entry))
                           (is-dir? (second entry))
                           (selector (third entry)))
                      (if is-dir?
                          (menu-item 'menu filename selector hostname port)
                          (menu-item-file (make-pathname local-path filename)
                                          filename
                                          selector
                                          hostname
                                          port)))))
              (cases Result item
                (Ok (v) (loop (cdr entries) (cons v res)))
                (Error (e) item) ) ) ) ) ) ) )


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


;; Process a NEX style index file.  This describes a menu and returns a list
;; of menu items.
;; TODO: Handle an empty index
;; TODO: Rename index to something else
(define (process-nex-index context selector root-dir local-path nex-index)

  (define (dir-item path username)
    (if (absolute-pathname? path)
        (menu-item 'menu username (trim-selector path)
                   (context-hostname context) (context-port context))
        (let ((item-selector (sprintf "~A~A~A"
                                      selector
                                      (if (string=? selector "") "" "/")
                                      (string-chomp path "/"))))
          (menu-item 'menu username item-selector
                     (context-hostname context) (context-port context)))))

  ;; TODO: Handle file not existing
  (define (file-item path username)
    (if (absolute-pathname? path)
        (menu-item-file (make-pathname root-dir (trim-selector path))
                        username
                        (trim-selector path)
                        (context-hostname context)
                        (context-port context))
        (let ((item-selector (sprintf "~A~A~A"
                                      selector
                                      (if (string=? selector "") "" "/")
                                      path)))
          (menu-item-file (make-pathname local-path (trim-selector path))
                          username
                          item-selector
                          (context-hostname context)
                          (context-port context)))))

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
               (menu-item-url (context-hostname context)
                              (context-port context)
                              username
                              path))
             ((is-dir? path)
               (dir-item path chomped-username))
             (else
               (file-item path username))))
          ;; Current selector is used for info itemtype so that if type
          ;; not supported by client but still displayed then it
          ;; will just link to the page that it is being displayed on
          (menu-item 'info line selector
                     (context-hostname context) (context-port context) ) ) ) )

  (let ((lines (string-split (string-trim-both nex-index char-set:whitespace) "\n" #t)))
    (let loop ((lines lines) (res '{}))
      (if (null? lines)
          ;; TODO: Is there a quicker way of doing this rather than using reverse?
          (Ok (reverse res))
          (let ((item (parse-line (car lines))))
            (cases Result item
              (Ok (v) (loop (cdr lines) (cons v res)))
              (Error (e) (Error-wrap item "error processing index") ) ) ) ) ) ) )

