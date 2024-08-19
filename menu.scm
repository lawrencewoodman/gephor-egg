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
;; descriptive itemtypes supported: ((text |0|) (menu |1|) (error |3|)
;;                                   (binhex |4|) (binary |9|) (info i)
;;                                   (html h) (gif g) (image I))
;;
;; |5| Dos Binary itemtype not recommended as it is unclear what this
;;     is and it should be able to be replaced by |9| in every instance.
;;
;; logs a warning if usernames > 69 characters as per RFC 1436
;;
;; Returns a Result type
;; It will return an Error of an unknown itemtype is used which is
;; longer than 1 character
(: menu-item (symbol string string string fixnum --> menu-item))
(define (menu-item itemtype username selector hostname port)
  (let ((username (string-trim-right username char-set:whitespace))
        (selector (string-trim-both selector char-set:whitespace)))
    (if (> (string-length username) 69)
        (log-warning "username: ~A, selector: ~A, hostname: ~A, port: ~A, username > 69 characters"
                  username selector hostname port))
    ;; TODO: Add more items
    (case itemtype
      ((text |0|)   (Ok (list "0" username selector hostname port)))
      ((menu |1|)   (Ok (list "1" username selector hostname port)))
      ((error |3|)  (Ok (list "3" username selector hostname port)))
      ((binhex |4|) (Ok (list "4" username selector hostname port)))
      ((binary |9|) (Ok (list "9" username selector hostname port)))
      ((info i)     (Ok (list "i" username selector hostname port)))
      ((html h)     (Ok (list "h" username selector hostname port)))
      ((gif g)      (Ok (list "g" username selector hostname port)))
      ((image I)    (Ok (list "I" username selector hostname port)))
      (else
        (let ((maybe-itemtype (symbol->string itemtype)))
          (if (= (string-length maybe-itemtype) 1)
              (Ok (list maybe-itemtype username selector hostname port))
              (Error-fmt "unknown itemtype: ~A" itemtype) ) ) ) ) ) )


;; Creates a menu item for a file.
;; local-path is the path to the file whose itemtype will be determined
;; using libmagic.
;;
;; Returns a Result type
(: menu-item-file (string string string --> (struct Result)))
(define (menu-item-file local-path username selector)
  ;; TODO: Check local-path is safe
  ;; TODO: check if a directory?
  (if (not (file-exists? local-path))
      (Error-fmt "local-path: ~A, file doesn't exist" local-path)
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
                (menu-item itemtype
                           username
                           selector
                           (server-hostname)
                           (server-port))))
            (Error-fmt "local-path: ~A, file type check failed" local-path) ) ) ) )



;; Supporters protocols: gopher ssh http https
;; TODO: Expand list of supported protocols
;; Clients that don't support the URL: prefix selectors and 'h'
;; itemtype should still work as they will go to the selector beginning
;; with URL: on our server and be served an HTML page which points
;; to the desired URL.
;; This is currently used by ssh, http and https and conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;;
;; Returns a Result type
;; Returns an Error if protocol is unknown
(: menu-item-url (string string --> (struct Result)))
(define (menu-item-url username url)
  (let-values (((protocol host port path itemtype) (split-url url)))
    (case (string->symbol protocol)
      ((gopher)
        ;; Gopher URLs should conform to RFC 4266
        (let ((itemtype (if itemtype (string->symbol itemtype) '|1|)))
          (menu-item itemtype username path host (or port 70))))
      ((ssh http https)
        (menu-item 'html
                   username
                   (sprintf "URL:~A" url)
                   (server-hostname)
                   (server-port)))
      (else
        (Error-fmt "url: ~A, unsupported protocol: ~A" url protocol) ) ) ) )


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
;; The selector isn't included in the error menu item in case that
;; could lead to an attack on the client.
(define (make-rendered-error-menu request msg)
  (let ((item (menu-item 'error msg "" (server-hostname) (server-port))))
    (cases Result item
      (Ok (v) (menu-render (list v)))
      (Error (e) (error* 'menu-item-rendered-error-menu e) ) ) ) )


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

