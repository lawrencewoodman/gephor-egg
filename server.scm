;;; A Gopher server module based on NEX for Chicken Scheme
;;;
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; TODO: rename module
;; TODO: rename  exported functions to make consistent and more predictable?
(module gophser
  (start-server
   make-context make-request
   make-router router-add router-match
   menu-item menu-item-info-wrap menu-item-file menu-item-url
   menu-render
   menu-ext-itemtype-map  ; TODO: Find a better way of handling this
   serve-url
   serve-path)

(import scheme
        (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken format)
        (chicken file)
        (chicken file posix)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken port)
        (chicken process signal)
        (chicken sort)
        (chicken string)
        (chicken tcp)
        (chicken type)
        fmt
        queues
        simple-logger
        srfi-1
        srfi-13
        srfi-14
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-1  - List procedures
;; srfi-13 - String library
;; srfi-14 - Character set library
;; srfi-18 - Multithreading support
;; queues  - In the source code it says that the procedures used
;;           here are thread safe


;; Record types -------------------------------------------------------------

;; TODO: should this be exported just so can be used in tests?
;; TODO: would it be better to put in another file that can be included by
;; TODO: the module and the tests?
(define-record-type context
  (make-context hostname port)
  context?
  (hostname context-hostname)
  (port context-port)
)


;; TODO: should this be exported just so can be used in tests?
(define-record-type request
  (make-request selector client-address)
  request?
  (selector request-selector)
  (client-address request-client-address)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Main Server Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Should this run as a thread and return something to allow
;; TODO: control of it?
(define (start-server hostname port router)
  (let ((requests (make-queue))
        (connects (make-queue))
        (context (make-context hostname port)))

  (define (termination-handler signum)
    ;; TODO: What else should this do
    ;; TODO: What message should be reported?
    ;; TODO: Should the message go to stderr and could it be better?
    ;; TODO: How would we handle this if this becomes a module rather than a
    ;; TODO: program?
    (write-line "server terminated")
    (exit))

  ;; TODO: Add timeout
  ;; TODO: Test this
  ;; Read the selector and trim from the beginning and the end whitespace
  ;; and '/' characters
  (define (read-selector in)
    (trim-selector (read-line in 255) ) )

  (define (client-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (log-info "client address: ~A, connection request" client-address)
      ;; TODO: Does in need to be in this and could it be closed first?
      ;; TODO: Find  a neater way of doing this
      (queue-add! connects (list (cons 'in in) (cons 'out out)
                                 (cons 'client-address client-address) ) ) ) )


  ;; TODO: handle timeout
  ;; TODO: Need to handle handlers failing
  (define (handle-connect connect)
    (let ((in (alist-ref 'in connect))
          (out (alist-ref 'out connect))
          (client-address (alist-ref 'client-address connect)))
      (let* ((selector (read-selector in))
             (handler (router-match router selector))
             (request (make-request selector client-address)))
        (write-string (if handler
                          (handler context request)
                          (begin
                            (log-warning "client address: ~A, selector: ~A, no handler for selector"
                                         client-address
                                         selector)
                            (make-rendered-error-menu context request "path not found")))
                      #f
                      out)
        (close-input-port in)
        (close-output-port out ) ) ) )


  (define (start-connect-handler-thread)
    (thread-start!
      (make-thread
        (lambda ()
          (let loop ()
            (handle-exceptions ex
              (begin
                (log-error "connect handler thread: ~A"
                           (get-condition-property ex 'exn 'message))
                (signal ex))
              (if (not (queue-empty? connects))
                (let* ((connect (queue-remove! connects)))
                  (handle-connect connect))
                  ;; The thread-sleep! is to prevent the server from
                  ;; continually polling when the connects queue is empty
                  ;; TODO: Find a better way of doing this perhaps using a mutex
                  ;; TODO: and altering it on client-connect
                  (thread-sleep! 0.1) ) )
            (loop) ) ) ) ) )


  ;; Returns a list of threads
  (define (start-connect-handler-threads num-threads)
    (let loop ((n num-threads) (threads '()))
      (if (= n 0)
          threads
          (let ((thread (start-connect-handler-thread)))
            (loop (- n 1) (cons thread threads) ) ) ) ) )



  ;; TODO: should we also set on-exit to do something?
  (define (set-termination-handler)
    (set-signal-handler! signal/hup termination-handler)
    (set-signal-handler! signal/int termination-handler)
    (set-signal-handler! signal/term termination-handler) )


  (let ((listener (tcp-listen port)))
    (set-termination-handler)
    (start-connect-handler-threads 10)
    (let loop ()
      (let-values ([(in out) (tcp-accept listener)])
        (client-connect in out))
      (loop) ) ) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Router
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Create a types file
(define-type router (list-of (pair string procedure)))


;; Exported Definitions ------------------------------------------------------

;; TODO: Add args to type def
;; TODO: typedef procedure?
(: make-router ((list-of (pair string procedure)) --> router))
(define (make-router . args)
  (router-sort-routes args))


;; TODO: check the pattern doesn't already exist
;; TODO: error if multiple * or * not at end
(: router-add (router string procedure --> router))
(define (router-add router pattern proc)
  (let ((route (cons pattern proc)))
    ; Add the route to the list of routes and resort
    (router-sort-routes (cons route router) ) ) )


;; TODO: should this return the pattern as well, perhaps using values?
;; TODO: define the procedure argument types?
(: router-match (router string --> (or procedure boolean)))
(define (router-match router selector)
  (let loop ((routes router))
    (and (not (null? routes))
         (let* ((route (car routes))
                (pattern (car route))
                (proc (cdr route))
                (splat-index (substring-index "*" pattern)))
          (if splat-index
              (if (substring=? selector pattern 0 0 splat-index)
                  proc
                  (loop (cdr routes)))
              (if (string=? selector pattern)
                  proc
                  (loop (cdr routes) ) ) ) ) ) ) )
                   


;; Internal Definitions ------------------------------------------------------

;; Sort routes in descending order of length of string before '*'
;; so that most general matches are at the end of the list.  If two
;; patterns are the same length sort in lexical order
;; TODO: rewrite explanation as it isn't quite right
(: router-sort-routes (router --> router))
(define (router-sort-routes router)
  (sort router
        (lambda (a b)
          (let ((a-pat (car a))
                (b-pat (car b)))
           (let ((splat-index-a (substring-index "*" a-pat))
                 (splat-index-b (substring-index "*" b-pat)))
             (if splat-index-a
                 (if (not splat-index-b)
                     #f
                     (if (= splat-index-a splat-index-b)
                         (string<? a-pat b-pat)
                         (< splat-index-b splat-index-a)))
                 (if splat-index-b
                     #t
                     (string<? a-pat b-pat) ) ) ) ) ) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Route Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exported Definitions ------------------------------------------------------

;; Serve an html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; TODO: rename
;; TODO: Allow customisation such as passing in a different template?
;; TODO: test
(define (serve-url request)
  (if (not (substring=? (request-selector request) "URL:"))
      (begin
        (log-error "client address: ~A, selector: ~A, handler: serve-url, invalid selector"
                   (request-client-address request) (request-selector request))
        (make-rendered-error-menu context request "server error"))
      (let* ((url (substring (request-selector request) 4)))
        (log-info "client address: ~A, selector: ~A, handler: serve-url, url: ~A"
                  (request-client-address request)
                  (request-selector request)
                  url)
        (string-translate* url-html-template (list (cons "@URL" url) ) ) ) ) )


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
  (define (log-error* . args)
    (apply log-error (conc "client address: ~A, selector: ~A, handler: serve-path, " (car args))
                     (request-client-address request) (request-selector request)
                     (cdr args)))

  ;; TODO: Rename local-path ?
  ;; TODO: Check selector and path are safe
  ;; TODO: Find a better way of reducing safety check of root-dir and local-path
  (let* ((local-path (make-pathname root-dir (request-selector request))))
    (cond ((or (not (absolute-pathname? root-dir))
               (substring-index "/" root-dir (sub1 (string-length root-dir))))
            ;; TODO: Improve this error / exception
            (log-error* "root-dir isn't valid: ~A" root-dir)
            (make-rendered-error-menu context request "server error"))
          ((unsafe-pathname? local-path)   ;; TODO: Is this right for NEX style now?
            (log-warning* "selector isn't safe")
            (make-rendered-error-menu context request "invalid selector"))
          ((not (file-exists? local-path))
            (log-warning* "local path doesn't exist: ~A" local-path)
            (make-rendered-error-menu context request "path not found"))
          ((not (world-readable? local-path))
            (log-warning* "local path isn't world readable: ~A" local-path)
            (make-rendered-error-menu context request "path not found"))
          ;; TODO: allow or don't allow gophermap to be downloaded?
          ((regular-file? local-path)
            (handle-exceptions ex
              (begin
                (log-error* "error reading file: ~A, ~A"
                           local-path
                           (get-condition-property ex 'exn 'message))
                (make-rendered-error-menu context request "server error"))
              (log-info* "request file: ~A" local-path)
              (read-file local-path)))
          ((directory? local-path)
            (handle-exceptions ex
              (begin
                (log-error* "procedure: ~A, local-path: ~A, error listing directory: ~A"
                            (get-condition-property ex 'exn 'location "?")
                            local-path
                            (get-condition-property ex 'exn 'message))
                (make-rendered-error-menu context request "server error"))
              (log-info* "list directory: ~A" local-path)
              (if (file-exists? (make-pathname local-path "index"))
                  ;; TODO: Check index is world readable and otherwise suitable
                  ;; TODO: Do we want to use 'index' as filename?
                  (let ((nex-index (read-file (make-pathname local-path "index"))))
                    (menu-render (process-nex-index context
                                                    (request-selector request)
                                                    local-path
                                                    nex-index)))
                  (menu-render (list-dir context (request-selector request) local-path)))))
          (else
            (log-error* "unsupported file type for path: ~A" local-path)
            (make-rendered-error-menu context request "path not found") ) ) ) )



;; Internal Definitions ------------------------------------------------------


;; Does the file have word readable permissions?
(define (world-readable? filename)
  (= perm/iroth (bitwise-and (file-permissions filename) perm/iroth)))


;; NOTE: Reads a maximum of 50Mb - this could be set in context
;; TODO: Do we want to choose a different maximum read size?
;; TODO: Log an error if bigger than maximum size - perhaps
;; TODO: check this first and send an error to client if too big
(define (read-file path)
  (let ((contents (call-with-input-file path
                    (lambda (port) (read-string 50000000 port))
                    #:binary)))
  (if (eof-object? contents) "" contents) ) )


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
;; TODO: Make sure paths are safe
;; TODO: Should this check if path is world-readable rather than calling proc?
;; NOTE: selector-subpath must be checked to be safe before calling list-dir
;; TODO: Could we just pass selector into this rather than prefix and subpath ??
(define (list-dir context selector local-path)
  ;; Returns #f if not a valid file
  ;; An entry consists of a list (filename is-directory selector)
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

  (let ((filenames (directory local-path))
        (hostname (context-hostname context))
        (port (context-port context)))
    (map (lambda (entry)
           (let ((filename (car entry))
                 (is-dir (second entry))
                 (selector (third entry)))
             ;; TODO: Need to support other itemtypes
             (if is-dir
                 (menu-item 'menu filename selector hostname port)
                 (menu-item-file menu-ext-itemtype-map filename selector hostname port))))
         (sort-dir-entries (filter-map make-dir-entry filenames) ) ) ) )


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type menu-item (list string string string string fixnum))

;; Exported Definitions ------------------------------------------------------


;; Add an item to a menu
;; itemtype is a symbol and therefore numbers must be escaped
;; Warns if usernames > 69 characters as per RFC 1436
;; TODO: Should this be using signal or abort?
;; TODO: Could this use symbols rather than strings for long names?
(: menu-item (string string string string fixnum --> menu-item))
(define (menu-item itemtype username selector hostname port)
  (let ((username (string-trim-right username char-set:whitespace))
        (selector (string-trim-both selector char-set:whitespace)))
    (if (>= (string-length username) 70)
        ;; TODO: Test
        (log-info "proc: menu-item, username: ~A, selector: ~A, hostname: ~A, port: ~A, username >= 70 characters"
                  username selector hostname port))
    ;; TODO: simplify this and add more items
    (cond ((memq itemtype '(text |0|))
            (list "0" username selector hostname port))
          ((memq itemtype '(menu |1|))
            (list "1" username selector hostname port))
          ((memq itemtype '(error |3|))
            (list "3" username selector hostname port))
          ((memq itemtype '(binary |9|))
            (list "9" username selector hostname port))
          ((memq itemtype '(info i))
            (list "i" username selector hostname port))
          ((memq itemtype '(html h))
            (list "h" username selector hostname port))
          ((memq itemtype '(image I))
            (list "I" username selector hostname port))
          (else
            ;; TODO: Should this use the item type regardless if supplied as
            ;; TODO: long as it is a single character, and then just log a
            ;; TODO: warning?
            (signal (sprintf "menu-item: unknown item type: ~A" itemtype) ) ) ) ) )


;; TODO: Should we pass context rather than supplying hostname and port
;; TODO: Check if this works with non POSIX style paths
;; TODO: Document handled extensions here and where it is called such as
;; TODO: list-dir and process-nex-index
;; TODO: Pass an extention to itemtype map
;; TODO: What to do with unhandled type? Test
;; TODO: Test when file has no extension
;; Creates a menu item for a file.  The itemtype is determined by looking at
;; the file extension in the selector.  Extensions are mapped to itemtypes
;; using extension-map which is an alist of symbol pairs with the keys being
;; extensions and the values being itemtypes.  If an extension is unknown
;; a warning message is logged and 'text is used.
(define (menu-item-file extension-map username selector hostname port)
  (let* ((extension (string->symbol (string-downcase (or (pathname-extension selector) ""))))
         (maybe-itemtype (alist-ref extension extension-map)))
    (if maybe-itemtype
        (menu-item maybe-itemtype username selector hostname port)
        (begin
          (log-warning "username: ~A, selector: ~A, extension: ~A, proc: menu-item-file, unknown extension"
                       username selector extension)
          (menu-item 'text username selector hostname port) ) ) ) )


;; Supporters protocols: gopher ssh http https
;; TODO: Expand list of supported protocols
;; our-hostname and our-port refer to the hostname and port our server
;; is using.  This is used to point back to our server when using URL:
;; 'h' itemtype selectors.
(define (menu-item-url our-hostname our-port username url)
  (let-values (((protocol host port path itemtype) (split-url url)))
    (case (string->symbol protocol)
      ((gopher)
        ;; Gopher URLs should conform to RFC 4266
        (let ((itemtype (if itemtype (string->symbol itemtype) '|1|)))
          (menu-item itemtype username path host (or port 70))))
      ((ssh http https)
        ;; Conforms to: gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
        ;; 'host' and 'port' point to the gopher server that provided the
        ;; directory this is to support clients that don't support the
        ;; URL: prefix.  These clients should be served a HTML page which points
        ;; to the desired URL.
        (menu-item 'html username (sprintf "URL:~A" url) our-hostname our-port) )
      (else
        ;; TODO: Test this
        (log-error "proc: menu-item-url, username: ~A, url ~A, protocol: ~A, unsupported protocol"
                   username url protocol) ) ) ) )


;; Takes a username and wraps it at the 69th column, as per RFC 1436, to
;; return a list of menu items
(: menu-item-info-wrap (string string string fixnum --> (listof menu-item)))
(define (menu-item-info-wrap username selector hostname port)
  ;; TODO: Should we allow some lines to be unwrappable if they don't
  ;; TODO: contain spaces.
  ;; TOOD: Should we split the text first and then wrap an then split again
  ;; TODO: to allow newlines to be used in the source text?
  ;; TODO: Rewrite a wrap func so don't need to bring in
  ;; TODO: big fmt and associated packages
  ;; TODO: escape characters in username such as \t or strip out?
  (let ((lines (string-split (fmt #f (with-width 69 (wrap-lines username))) "\n")))
    (map (lambda (line) (menu-item 'info line selector hostname port)) lines) ) )


;; Render the menu as text ready for sending
(: menu-render ((list-of menu-item) --> string))
(define (menu-render menu-items)
  (define (item-render item)
    (apply sprintf "~A~A\t~A\t~A\t~A\r\n" item))

  (let ((menu-str
          (foldl (lambda (out-str item)
                   (string-append out-str (item-render item)))
                 ""
                 menu-items)))
    ;; Properly constructed menus should end with ".\r\n"
    (string-append menu-str ".\r\n") ) )


;; Make an error menu that has been rendered and is ready for sending
;; TODO: Is it a good idea / safe to return the selector?
(define (make-rendered-error-menu context request msg)
  (menu-render (list (menu-item 'error msg (request-selector request)
                                 (context-hostname context)
                                 (context-port context) ) ) ) )


;; Internal Definitions ------------------------------------------------------

;; Compiled Regular Expressions to split URLs
(define url-split-regex (string->irregex "^(.*):\/\/([^:/]+)(:([0-9]*))?(.*)$"))
(define gopher-path-split-regex (string->irregex "^\/(.)(.*)$"))


;; Split up a URL to return values for: protocol host port path itemtype
;; port will be #f unless present
;; path will be "" if not present
;; itemtype will be #f unless protocol is gopher or gophers and URL has a path
(define (split-url url)
  (let ((url-match (irregex-search url-split-regex url)))
    (if (irregex-match-data? url-match)
        (let ((protocol (string-downcase (irregex-match-substring url-match 1)))
              (host (irregex-match-substring url-match 2))
              (port (irregex-match-substring url-match 4))
              (path (or (irregex-match-substring url-match 5) "")))
          (if (member protocol '("gopher" "gophers"))
              (let ((itemtype-path-match (irregex-search gopher-path-split-regex path)))
                (if (irregex-match-data? itemtype-path-match)
                    (let ((itemtype (or (irregex-match-substring itemtype-path-match 1) ""))
                          (path (or (irregex-match-substring itemtype-path-match 2) "")))
                      (values protocol host port path itemtype))
                    (values protocol host port path #f)))
              (values protocol host port path #f)))
        #f) ) )


;; TODO: Where should this go?
;; alist mapping extensions to itemtypes
;; NOTE: This should be keep in order of most frequent lookup to make
;; NOTE: lookups as quick as possible on average.
;; NOTE: 'text itemtypes are included to prevent too many warnings about
;; NOTE: unknown extensions in menu-item-file
(define menu-ext-itemtype-map '(
  (txt . text) (c . text) (cpp . text) (go . text) (md . text) (py . text) (tcl . text)
  (gif . gif)
  (bmp . image)  (jpg . image) (jpeg . image) (png . image) (tif . image) (rle . image)
  (html . html) (htm . html) (xhtml . html)
  (mkv . binary) (mp4 . binary) (avi . binary)
))


;; TODO: Rethink this
(define (valid-ext-itemtype-map? x)
  (let* ((known-itemtypes '(binary html image gif text ))
         (unknown-itemtype? (lambda (itemtype) (not (memq itemtype known-itemtypes))))
         (keys (map car x))
         (values (map cdr x)))
    (and (= (length keys) (length (delete-duplicates keys)))
         (= 0 (count unknown-itemtype? values) ) ) ) )
(assert (valid-ext-itemtype-map? menu-ext-itemtype-map))






;; TODO: Rename index to something else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEX Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exported Definitions ------------------------------------------------------

;; TODO: Handle an empty index
;; TODO: Remove blank lines at end
(define (process-nex-index context selector local-path nex-index)

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
        (menu-item-file menu-ext-itemtype-map username (trim-selector path)
                        (context-hostname context) (context-port context))
        (let ((item-selector (sprintf "~A~A~A"
                                      selector
                                      (if (string=? selector "") "" "/")
                                      path)))
          (menu-item-file menu-ext-itemtype-map username item-selector
                          (context-hostname context) (context-port context)))))

  (define (is-dir? path)
    (substring-index "/" path (sub1 (string-length path))))

  (define (is-url? path)
    (let ((url-match (irregex-match url-regex path)))
      (irregex-match-data? url-match)))


  (let ((lines (string-split (string-trim-both nex-index char-set:whitespace) "\n" #t)))
    (map (lambda (line)
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
                    ((is-dir? path) (dir-item path chomped-username))
                    (else (file-item path username))))
                 ;; Current selector is used for info itemtype so that if type
                 ;; not supported by client but still displayed then it
                 ;; will just link to the page that it is being displayed on
                 (menu-item 'info line selector
                            (context-hostname context) (context-port context)))))
         lines) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Internal Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A char set for trimming selectors
(define selector-trim-char-set
  (char-set-adjoin char-set:whitespace #\/) )

;; Trim beginning and end of selector to remove whitespace and
;; '/' characters
(define (trim-selector selector)
  (string-trim-both selector selector-trim-char-set) )

;; Regular expression to split a => style link
(define index-link-split-regex (string->irregex "^[ ]*=>[ ]+([^ ]+)[ ]*(.*)$"))

;; Regular expression to identify a URL in a => style link
(define url-regex (string->irregex "^.*:\/\/[^:/]+(:[0-9]*)?.*$"))


)

