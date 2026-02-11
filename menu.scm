;;; Procedures for creating Gopher menus
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   menu-item menu-item-info-wrap menu-item-file menu-item-url
;;;   menu-render
;;;
;;; Copyright (C) 2024-2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
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
;; descriptive itemtypes supported: ((text |0|) (menu |1|) (ccso |2|)
;;                                   (error |3|) (binhex |4|) (dosbin |5|)
;;                                   (uue |6|) (search |7|) (telnet |8|)
;;                                   (binary |9|) (info i) (html h)
;;                                   (redundant +) (tn3270 T) (gif g)
;;                                   (image I))
;; |5| Dos binary archive itemtype is not recommended as it is unclear what
;;     this is and it should be able to be replaced by |9| in every instance.
;;
;; Returns #f if itemtype > 1 character and unknown,
;; otherwise a menu-item is returned
(: menu-item (symbol string string string fixnum --> (or menu-item false)))
(define (menu-item itemtype username selector hostname port)
  (let ((username (string-trim-right username char-set:whitespace))
        (selector (string-trim-both selector char-set:whitespace))
        (itemtype-char
          (case itemtype
            ((text |0|)    "0")
            ((menu |1|)    "1")
            ((ccso |2|)    "2")
            ((error |3|)   "3")
            ((binhex |4|)  "4")
            ((dosbin |5|)  "5")
            ((uue |6|)     "6")
            ((search |7|)  "7")
            ((telnet |8|)  "8")
            ((binary |9|)  "9")
            ((info i)      "i")
            ((html h)      "h")
            ((redundant +) "+")
            ((tn3270 T)    "T")
            ((gif g)       "g")
            ((image I)     "I")
            (else
              (let ((maybe-itemtype (symbol->string itemtype)))
                (and (= (string-length maybe-itemtype) 1)
                     maybe-itemtype))))))
    (and itemtype-char
        (list itemtype-char username selector hostname port) ) ) )


;; Creates a menu item for a file.
;; local-path is the path to the file whose itemtype will be determined
;; using libmagic unless it is a directory.
;;
;; Returns #f if the file doesn't exist or the type can't be determined,
;; otherwise a menu-item is returned
;;
;; Logs error messgaes for file doesn't exist or type can't be determined
(: menu-item-file (string string string -> (or menu-item false)))
(define (menu-item-file local-path username selector)
  (if (file-exists? local-path)
      (if (directory? local-path)
          (menu-item 'menu username selector (server-hostname) (server-port))
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
                (begin
                  (apply log-error
                         "unknown file type"
                         (cons 'local-path local-path)
                         (log-context))
                  #f))))
      (begin
        (apply log-error
               "file doesn't exist"
               (cons 'local-path local-path)
               (log-context))
        #f) ) )


;; Turn a URL into a menu item
;;
;; A gopher URL will be interpreted and point directly to the menu
;; indicated within the URL.  The URL must confirm to RFC 4266.
;;
;; A telnet URL will use the '8' itemtype unless it contains
;; a user, in which case it will use the 'h' itemtype and
;; URL: selector mentioned below.  Telnet URLs must conform to RFC 4248.
;;
;; All other URL schemes will create a menu item with the 'h' itemtype and
;; the selector will begin with 'URL: ' followed by the URL.  This will still
;; work with clients that don't support this as they can be served an HTML
;; page which points to the desired URL.  This conforms to:
;;   gopher://bitreich.org:70/1/scm/gopher-protocol/file/references/h_type.txt.gph
;;
;; Returns #f if URL is invalid otherwise a menu-item is returned
(: menu-item-url (string string --> (or menu-item false)))
(define (menu-item-url username url)
  (let-values (((scheme userinfo host port path itemtype) (split-url url)))
    (and scheme
         (case (string->symbol scheme)
           ((gopher)
             (let ((itemtype (if itemtype (string->symbol itemtype) '|1|)))
               (menu-item itemtype username path host (or port 70))))
           ((telnet)
             (if userinfo
                 (menu-item 'telnet
                            username
                            (sprintf "URL:~A" url)
                            (server-hostname)
                            (server-port))
                 (menu-item '|8| username path host (or port 23))))
           (else
             (menu-item 'html
                        username
                        (sprintf "URL:~A" url)
                        (server-hostname)
                        (server-port) ) ) ) ) ) )


;; Render the menu as text ready for sending
(: menu-render ((list-of menu-item) --> string))
(define (menu-render menu-items)
  (let ((menu-str
          (foldl (lambda (out-str item)
                   (string-append out-str (render-menu-item item)))
                 ""
                 menu-items)))
    ;; Properly constructed menus should end with ".\r\n"
    (string-append menu-str ".\r\n") ) )


;; Internal Definitions ------------------------------------------------------

;; Compiled Regular Expressions to split URLs
(define url-split-regex (string->irregex "^(.+?):\/\/(.*?@)?([^:/]+)(:([0-9]*))?(.*)$"))
(define gopher-path-split-regex (string->irregex "^\/(.)(.*)$"))

;; Compiled Regular Expression to split magic file mime types
(define mime-split-regex (string->irregex "^([^/]+)\/([^;]+); charset=.*$"))

;; Render a menu item
(define (render-menu-item item)
  (apply sprintf "~A~A\t~A\t~A\t~A\r\n" item))


;; Split up a URL to return values for: scheme userinfo host port path itemtype
;; port will be #f unless present
;; path will be "" if not present
;; itemtype will be #f unless scheme is gopher or gophers and URL has a path
;; If a telnet URL, it conforms to RFC 4248 and will only allow a path of '/' or
;; none.  If any other path is used it will return #f.
(: split-url (string
              -->
              (or string false)
              (or string false)
              (or string false)
              (or string false)
              (or string false)
              (or string false)))
(define (split-url url)
  (let ((url-match (irregex-search url-split-regex url)))
    (if (irregex-match-data? url-match)
        (let ((scheme (string-downcase (irregex-match-substring url-match 1)))
              (userinfo (irregex-match-substring url-match 2))
              (host (irregex-match-substring url-match 3))
              (port (irregex-match-substring url-match 5))
              (path (or (irregex-match-substring url-match 6) "")))
          (case (string->symbol scheme)
            ((gopher gophers)
              (let ((itemtype-path-match (irregex-search gopher-path-split-regex path)))
                (if (irregex-match-data? itemtype-path-match)
                    (let ((itemtype (or (irregex-match-substring itemtype-path-match 1) ""))
                          (path (or (irregex-match-substring itemtype-path-match 2) "")))
                      (values scheme userinfo host port path itemtype))
                    (values scheme userinfo host port path #f))))
            ((telnet)
              (if (or (string=? path "") (string=? path "/"))
                  (values scheme userinfo host port "" #f)
                  (values #f #f #f #f #f #f)))
            (else
              (values scheme userinfo host port path #f))))
        (values #f #f #f #f #f #f) ) ) )

