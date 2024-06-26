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
  (start-server make-router router-add router-match serve-url)

(import scheme
        (chicken base)
        (chicken condition)
        (chicken format)
        (chicken io)
        (chicken port)
        (chicken process signal)
        (chicken sort)
        (chicken string)
        (chicken tcp)
        (chicken type)
        fmt
        queues
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-18 - Multithreading support
;; queues  - In the source code it says that the procedures used
;;           here are thread safe


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Main Server Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-server port router)
  (let ((requests (make-queue))
        (connects (make-queue))
        (context (list (cons 'port port))))

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
      (printf "CONNECT: client address: ~A~%~!" client-address)
      ;; TODO: Does in need to be in this and could it be closed first?
      ;; TODO: Find  a neater way of doing this
      (queue-add! connects (list (cons 'in in) (cons 'out out)
                                 (cons 'client-address client-address) ) ) ) )


  (define (handle-connect connect)
    (let ((in (cdr (assv 'in connect)))
          (out (cdr (assv 'out connect)))
          (client-address (cdr (assv 'client-address connect))))
      (let ((selector (read-selector in)))
        (printf "client address: ~A, selector: ~A~%~!" client-address selector)
        ;; TODO: Does in need to be in this and could it be closed first?
        ;; TODO: Find  a neater way of doing this
        (queue-add! requests (list (cons 'in in) (cons 'out out) 
                                   (cons 'client-address client-address)
                                   (cons 'selector selector) ) ) ) ) )


  (define (start-connect-handler-thread)
    (thread-start!
      (make-thread
        (lambda ()
          (let loop ()
            (handle-exceptions ex
              (signal ex)
              (if (not (queue-empty? connects))
                (let* ((connect (queue-remove! connects)))
                  (handle-connect connect) )
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
    (let* ((selector (cdr (assv 'selector request)))
           (in (cdr (assv 'in request)))
           (out (cdr (assv 'out request)))
           (client-address (cdr (assv 'client-address request)))
           ;; TOOD: Should handler be handler!
           (handler (router-match router selector)))
      (write-line (sprintf "hello client: ~A, asking for ~A~!" client-address selector) out) 
      (if handler
          (handler context request)
          (write-line (sprintf "selector not found") out))  ; TODO: return an error gophermap
      ;; TODO: When should these be closed?
      (close-input-port in)
      (close-output-port out) ) )
    

  ;; TODO: should this end in a !
  ;; TODO: should we have a way to tell thread to stop cleanly
  (define (start-request-handler-thread)
    (thread-start! 
      (make-thread 
        (lambda ()
          (let loop ()
            (handle-exceptions ex
              (signal ex)
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
              (if (equal? selector pattern)
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
(define (serve-url request)
  (let* ((selector (cdr (assv 'selector request)))
         (in (cdr (assv 'in request)))
         (out (cdr (assv 'out request))))
    (if (not (equal? (substring selector 0 4) "URL:"))
        ;; Log and error
        (printf "ERROR: for handler: serve-url, invalid selector: ~A~%~!" selector)
        (let* ((url (substring selector 4))
               (response (string-translate* url-html-template
                                            (list (cons "@URL" url)))))
          (write-string response #f out) ) ) ) )


;; Internal Definitions ------------------------------------------------------


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

;; TODO: define the type for menu
(define-type menu (list-of (pair string procedure)))

;; Exported Definitions ------------------------------------------------------

;; TODO: Add args to type def
;; TODO: typedef procedure?
(: make-menu (--> menu))
(define (make-menu)
  '())

;; Add an item to a menu
;; TODO: should itemtype be a symbol?
;; TODO: Create context typedef
(: menu-item (menu (list-of (pair symbol *)) string string string string fixnum --> menu))
(define (menu-item menu context itemtype username selector hostname port)
;; Add optional hostname and port?  (define (menu-item menu context itemtype username selector (hostname "") (port -1))
  (let ((hostname (if (equal? hostname "")
                      (cdr (assv 'hostname context))
                      hostname))
        (port (if (= port -1)
                  (cdr (assv 'port context))
                  port)))

    ;; Each item is added to the front of the list and therefore needs
    ;; reversing before rending
    ;; TODO: simplify this and add more items
    (case itemtype
      ('("text" "0")  (cons (list itemtype username selector hostname port) menu))
      ('("menu" "1")  (cons (list itemtype username selector hostname port) menu))
                      ;; TODO: is 80 the correct wrap width here?
                      ;; TODO: Should we allow some lines to be unwrappable if they don't
                      ;; TODO: contain spaces.
                      ;; TODO: What is best to put as the selector, host and port
                      ;; TOOD: Should we split the text first and then wrap an then split again
                      ;; TODO: to allow newlines to be used in the source text?
                      ;; TODO: Put info wrap in a seperate func
      ('("info" "i")  (let ((lines (string-split (fmt #f (with-width 80 (wrap-lines username)))
                                                 "\n")))
                        (foldl (lambda (line items)
                                 (cons (list itemtype line selector hostname port) items))
                               menu
                               lines)))
      ('("html" "h")  (cons (list itemtype username selector hostname port) menu))
      ('("image" "I") (cons (list itemtype username selector hostname port) menu))
      (else           (begin  ; TODO: rewrite this to handle error properly
                        (printf "ERROR: unkown item type: ~A~%~!" itemtype)
                        menu ) ) ) ) )


;; Internal Definitions ------------------------------------------------------


)

