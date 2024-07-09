;;; A Gopher server module for Chicken Scheme
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
   menu-item menu-item-info menu-render
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
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-1  - List procedures
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
  (make-request selector out-port client-address)
  request?
  (selector request-selector)
  (out-port request-out-port)
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
    

  ;; TODO: add timeout
  (define (read-selector in)
    (read-line in 255) )


  (define (client-connect in out)
    (let-values ([(_ client-address) (tcp-addresses in)])
      (log-info "client address: ~A, connection request" client-address)
      ;; TODO: Does in need to be in this and could it be closed first?
      ;; TODO: Find  a neater way of doing this
      (queue-add! connects (list (cons 'in in) (cons 'out out)
                                 (cons 'client-address client-address) ) ) ) )


  ;; TODO: handle timeout
  (define (handle-connect connect)
    (let ((in (cdr (assv 'in connect)))
          (out (cdr (assv 'out connect)))
          (client-address (cdr (assv 'client-address connect))))
      (let ((selector (read-selector in)))
        ;; TODO: When should these be closed?
        (close-input-port in)
        ;; TODO: Does in need to be in this and could it be closed first?
        ;; TODO: Find  a neater way of doing this
        (queue-add! requests (make-request selector out client-address) ) ) ) )


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


  ;; TODO: Need a way to handle different types of response: binary, text, etc
  ;; TODO: Need to handle handlers failing
  (define (handle-request request)
    ;; TOOD: Should handler be handler!
    (let ((handler (router-match router (request-selector request))))
      ;; TODO: Should we really be passing output port in request to handler?
      (write-string (if handler
                        (handler context request)
                        (begin
                          (log-warning "client address: ~A, selector: ~A, no handler for selector"
                                       (request-client-address request)
                                       (request-selector request) )
                          (make-rendered-error-menu context request "path not found")))
                    #f
                    (request-out-port request))
      (close-output-port (request-out-port request) ) ) )
    

  ;; TODO: should this end in a !
  ;; TODO: should we have a way to tell thread to stop cleanly
  (define (start-request-handler-thread)
    (thread-start! 
      (make-thread 
        (lambda ()
          (let loop ()
            (handle-exceptions ex
              (begin
                (log-error "request handler thread: ~A"
                           (get-condition-property ex 'exn 'message))
                (signal ex))
              (if (not (queue-empty? requests))
                  (let* ((request (queue-remove! requests)))
                    (handle-request request) )
                  ;; The thread-sleep! is to prevent the server from
                  ;; continually polling when the requests queue is empty
                  ;; TODO: Find a better way of doing this perhaps using a mutex
                  ;; TODO: and altering it on client-connect
                  (thread-sleep! 0.1) ) )
            (loop) ) ) ) ) )


  ;; Returns a list of threads
  (define (start-request-handler-threads num-threads)
    (let loop ((n num-threads) (threads '()))
      (if (= n 0)
          threads
          (let ((thread (start-request-handler-thread)))
            (loop (- n 1) (cons thread threads) ) ) ) ) )


  ;; TODO: should we also set on-exit to do something?
  (define (set-termination-handler)
    (set-signal-handler! signal/hup termination-handler)
    (set-signal-handler! signal/int termination-handler)
    (set-signal-handler! signal/term termination-handler) )


  (let ((listener (tcp-listen port)))
    (set-termination-handler)
    (start-connect-handler-threads 5)
    (start-request-handler-threads 5)
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

;; Serve a html page for cases when the selector begins with 'URL:' followed
;; by a URL.  This is for clients that don't support the 'URL:' selector
;; prefix so that they can be served a html page which points to the URL.
;; This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;; TODO: rename
;; TODO: Allow customisation such as passing in a different template?
;; TODO: test
(define (serve-url request)
  (if (not (string=? (substring (request-selector request) 0 4) "URL:"))
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


;; NOTE: When dealing with paths we must remember that a selector has no
;; NOTE: notion of a file path.  It is purely up to the server to decide
;; NOTE: how to understand it.
;;
;; local-dir must not end with a '/'
;; TODO: Test how this handles an empty selector, the same as "/"?
(define (serve-path context request local-dir selector-prefix)
  (define (valid-local-dir? dir)
    (and (absolute-pathname? dir)
         (not (substring-index "/" dir (sub1 (string-length dir))))))

  (let* ((selector-subpath (strip-selector-prefix selector-prefix (request-selector request)))
         ;; TODO: Rename local-path ?
         (local-path (path-build local-dir selector-subpath)))
    (cond ((not (valid-local-dir? local-dir))
            (log-error "client address: ~A, selector: ~A, handler: serve-path, local-dir isn't valid: ~A"
                     (request-client-address request) (request-selector request) local-dir)
            (make-rendered-error-menu context request "server error"))
          ((unsafe-pathname? selector-subpath)
            (log-warning "client address: ~A, selector: ~A, handler: serve-path, selector isn't safe"
                    (request-client-address request) (request-selector request))
            (make-rendered-error-menu context request "invalid selector"))
          ((not (file-exists? local-path))
            (log-warning "client address: ~A, selector: ~A, handler: serve-path, local path doesn't exist: ~A"
                    (request-client-address request) (request-selector request) local-path)
            (make-rendered-error-menu context request "path not found"))
          ((not (world-readable? local-path))
            (log-warning "client address: ~A, selector: ~A, handler: serve-path, local path isn't world readable: ~A"
                    (request-client-address request) (request-selector request) local-path)
            (make-rendered-error-menu context request "path not found"))
          ;; TODO: allow or don't allow gophermap to be downloaded?
          ((regular-file? local-path)
            (handle-exceptions ex
              (begin
                (log-error "client address: ~A, selector: %A, handler: serve-path, error reading file: ~A, ~A"
                           (request-client-address request)
                           (request-selector request)
                           local-path
                           (get-condition-property ex 'exn 'message))
                (make-rendered-error-menu context request "server error"))
              (log-info "client address: ~A, selector: ~A, handler: serve-path, request file: ~A"
                        (request-client-address request)
                        (request-selector request)
                        local-path)
              (read-file local-path)))
          ((directory? local-path)
            (handle-exceptions ex
              (begin
                (log-error "client address: ~A, selector: ~A, handler: serve-path, error listing directory: ~A, ~A"
                           (request-client-address request)
                           (request-selector request)
                           local-path
                           (get-condition-property ex 'exn 'message))
                (make-rendered-error-menu context request "server error"))
              (log-info "client address: ~A, selector: ~A, handler: serve-path, list directory: ~A"
                        (request-client-address request)
                        (request-selector request)
                        local-path)
              ;; TODO: make this look nicer
              ;; TODO: re-think these args
              (menu-render (list-dir context local-path selector-prefix selector-subpath))))
          (else
            (log-error "client address: ~A, selector: ~A, handler: serve-path, unsupported file type for path: ~A"
                     (request-client-address request) (request-selector request) local-path)
            (make-rendered-error-menu context request "path not found") ) ) ) )



;; Internal Definitions ------------------------------------------------------

;; TODO: Move this, export? and rename?
;; Returns the selector without the prefix or #f if the prefix isn't present
(define (strip-selector-prefix prefix selector)
  (let ((prefix-exists (substring=? prefix selector)))
    (if (not prefix-exists)
        #f
        (substring selector (string-length prefix)))))

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


;; TODO: Move this, export? and rename?
;; TODO: rename path-join?
;; TODO: Make this safer and more resiliant and specify needs posix support
;; TODO: Perhaps strip out any .. or . as a selector isn't a real path
(define (path-build . args)
  (string-intersperse args "/"))


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
(define (list-dir context selector-local-dir selector-prefix selector-subpath)
  ;; Returns #f if not a valid file
  ;; An entry consists of a list (filename is-directory selector)
  (define (filename->dir-entry filename)
    (let ((fullfilename (make-pathname selector-local-dir filename))
          (selector (sprintf "~A~A~A~A"
                             selector-prefix
                             selector-subpath
                             (if (string=? selector-subpath "") "" "/")
                             filename)))
      (cond ((directory? fullfilename)
              (list filename #t selector))
            ((regular-file? fullfilename)
              (list filename #f selector))
            (else #f))))

  (let ((filenames (directory selector-local-dir))
        (hostname (context-hostname context))
        (port (context-port context)))
    (map (lambda (entry)
           (let ((filename (car entry))
                 (is-dir (second entry))
                 (selector (third entry)))
             ;; TODO: Need to support other itemtypes
             (if is-dir
                 (menu-item "menu" filename selector hostname port)
                 (menu-item "text" filename selector hostname port))))
         (sort-dir-entries (filter-map filename->dir-entry filenames) ) ) ) )


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
;; TODO: Should this be using signal or abort?
(: menu-item (string string string string fixnum --> menu-item))
(define (menu-item itemtype username selector hostname port)
    ;; TODO: simplify this and add more items
    (cond ((member itemtype '("text" "0"))
            (list "0" username selector hostname port))
          ((member itemtype '("menu" "1"))
            (list "1" username selector hostname port))
          ((member itemtype '("error" "3"))
            (list "3" username selector hostname port))
          ((member itemtype '("info" "i"))
            (signal (sprintf "menu-item: unsupported item type: ~A, use menu-item-info" itemtype)))
          ((member itemtype '("html" "h"))
            (list "h" username selector hostname port))
          ((member itemtype '("image" "I"))
            (list "I" username selector hostname port))
          (else
            (signal (sprintf "menu-item: unknown item type: ~A" itemtype) ) ) ) )


;; Return a list of menu items
(: menu-item-info (string string string fixnum --> (listof menu-item)))
(define (menu-item-info username selector hostname port)
  ;; TODO: is 80 the correct wrap width here?
  ;; TODO: Should we allow some lines to be unwrappable if they don't
  ;; TODO: contain spaces.
  ;; TODO: What is best to put as the selector, host and port
  ;; TOOD: Should we split the text first and then wrap an then split again
  ;; TODO: to allow newlines to be used in the source text?
  ;; TODO: Rewrite a wrap func so don't need to bring in
  ;; TODO: big fmt and associated packages
  ;; TODO: escape characters in username such as \t ?
  (let ((lines (string-split (fmt #f (with-width 80 (wrap-lines username))) "\n")))
    (map (lambda (line) (list "i" line selector hostname port)) lines)))


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
    (string-append menu-str ".\r\n")))


;; Make an error menu that has been rendered and is ready for sending
;; TODO: Is it a good idea / safe to return the selector?
(define (make-rendered-error-menu context request msg)
  (menu-render (list (menu-item "error" msg (request-selector request)
                                 (context-hostname context)
                                 (context-port context) ) ) ) )

)

