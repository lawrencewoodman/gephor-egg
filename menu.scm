;;; Procedures for creating Gopher menus
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   menu-item menu-item-info-wrap menu-item-file menu-item-url
;;;   menu-render
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;

(define-type menu-item (list string string string string fixnum))

;; Exported Definitions ------------------------------------------------------


;; Create a menu item for the itemtype specified
;;
;; itemtype is a symbol and therefore numbers must be escaped.  It can use
;; the single letter from RFC 1436 or a descriptive symbol, i.e. for text
;; we can use 'text or '|0|
;; itemtypes supported: ((text |0|) (menu |1|) (error |3|)
;;                       (binhex |4|) (binary |9|) (info i) (html h)
;;                       (gif g) (image I))
;; |5| Dos Binary itemtype not supported as it is unclear what this
;;     is and should be able to be replaced by |9| in every instance.
;;
;; Returns a blank info type if itemtype is unknown
;;
;; Warns if usernames > 69 characters as per RFC 1436
(: menu-item (symbol string string string fixnum --> menu-item))
(define (menu-item itemtype username selector hostname port)
  (let ((username (string-trim-right username char-set:whitespace))
        (selector (string-trim-both selector char-set:whitespace)))
    (if (>= (string-length username) 70)
        ;; TODO: Test
        (log-info "proc: menu-item, username: ~A, selector: ~A, hostname: ~A, port: ~A, username >= 70 characters"
                  username selector hostname port))
    ;; TODO: Add more items
    (case itemtype
      ((text |0|)   (list "0" username selector hostname port))
      ((menu |1|)   (list "1" username selector hostname port))
      ((error |3|)  (list "3" username selector hostname port))
      ((binhex |4|) (list "4" username selector hostname port))
      ((binary |9|) (list "9" username selector hostname port))
      ((info i)     (list "i" username selector hostname port))
      ((html h)     (list "h" username selector hostname port))
      ((gif g)      (list "g" username selector hostname port))
      ((image I)    (list "I" username selector hostname port))
      (else
        (log-error "proc: menu-item, itemtype: ~A, username: ~A, selector: ~A, hostname: ~A, port: ~A, unknown itemtype"
                   itemtype username selector hostname port)
        ;; TODO: should this use our hostname and port rather than the intended target?
        (list "i" "" "" hostname port) ) ) ) )


;; TODO: Test file check fail
;; TODO: Should we pass context rather than supplying hostname and port
;; TODO: Check if this works with non POSIX style paths
;; Creates a menu item for a file.
;; local-path is the path to the file whose itemtype will be determined
;; using libmagic.
(: menu-item-file (string string string string fixnum --> menu-item))
(define (menu-item-file local-path username selector hostname port)
  ;; TODO: Check local-path is safe
  ;; TODO: Catch exceptions
  ;; TODO: test and handle files not present
  (let* ((mime-type (identify local-path 'mime))
         (mime-match (irregex-search mime-split-regex mime-type)))
    (if (irregex-match-data? mime-match)
        (let ((media-type (irregex-match-substring mime-match 1))
              (media-subtype (irregex-match-substring mime-match 2)))
          (let ((itemtype (cond
                            ((string=? media-type "image")
                               (if (string=? media-subtype "gif")
                                   'gif
                                   'image))
                            ((string=? media-type "text")
                               (if (string=? media-subtype "html")
                                   'html)
                                   'text)
                            (else 'binary))))
            (menu-item itemtype username selector hostname port)))
        (error* 'menu-item-file "local-path: ~A, file type check failed" local-path) ) ) )



;; Supporters protocols: gopher ssh http https
;; TODO: Expand list of supported protocols
;; our-hostname and our-port refer to the hostname and port our server
;; is using so that we can support clients that don't support the URL:
;; prefix selectors and 'h' itemtype.  These clients will go to the selector
;; beginning with URL: on our server and should be served an HTML page which points
;; to the desired URL.  This is currently used by ssh, http and https
;; and conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;;
;; Raises an exception if protocol is unknown
(: menu-item-url (string fixnum string string --> menu-item))
(define (menu-item-url our-hostname our-port username url)
  (let-values (((protocol host port path itemtype) (split-url url)))
    (case (string->symbol protocol)
      ((gopher)
        ;; Gopher URLs should conform to RFC 4266
        (let ((itemtype (if itemtype (string->symbol itemtype) '|1|)))
          (menu-item itemtype username path host (or port 70))))
      ((ssh http https)
        (menu-item 'html username (sprintf "URL:~A" url) our-hostname our-port) )
      (else
        ;; TODO: Test this
        (error* 'menu-item-url "url: ~A, unsupported protocol: ~A" url protocol) ) ) ) )


;; Takes a username and wraps it at the 69th column, as per RFC 1436, to
;; return a list of menu items
(: menu-item-info-wrap (string string string fixnum --> (list-of menu-item)))
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

;; Compiled Regular Expression to split magic file mime types
(define mime-split-regex (string->irregex "^([^/]+)\/([^;]+); charset=.*$"))

;; Split up a URL to return values for: protocol host port path itemtype
;; port will be #f unless present
;; path will be "" if not present
;; itemtype will be #f unless protocol is gopher or gophers and URL has a path
(: split-url (string
              -->
              (or string false)
              (or string false)
              (or string false)
              (or string false)
              (or string false)))
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
        (values #f #f #f #f #f) ) ) )

