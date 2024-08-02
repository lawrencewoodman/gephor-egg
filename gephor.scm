;;; A Gopher server module for Chicken Scheme
;;;
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; TODO: rename  exported functions to make consistent and more predictable?
(module gephor
  (start-server stop-server
   make-context make-request
   make-router router-add router-match
   menu-item menu-item-info-wrap menu-item-file menu-item-url
   menu-render
   serve-url
   serve-path
   Result Ok Error)

;; TODO: Should Ok and Error be exported as Result-Ok, Result-Error?

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
        datatype
        fmt
        queues
        magic
        simple-logger
        srfi-1
        srfi-13
        srfi-14
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-1   - List procedures
;; srfi-13  - String library
;; srfi-14  - Character set library
;; srfi-18  - Multithreading support
;; datatype - Variant records
;; queues   - In the source code it says that the procedures used
;;            here are thread safe
;; magic    - Magic file type recognition


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



;; Algebraic Data Types -----------------------------------------------------

;; TODO: Test all this properly
;; TODO: Confirm test will use Ok and Error as results to test against accurately

;; Exported
(define-datatype Result Result?
  (Ok (v any?))
  (Error (e list-of-string?) ) )


;; Used to allow Ok to contain anything
(define (any? x)
  #t)

(define list-of-string? (list-of? string?))

; Pass the arguments to sprintf to create the error string making it easy to
; create a formatted erro string
(define (Error-fmt . args)
  (Error (list (apply sprintf args) ) ) )

(define (Error-wrap err . args)
  (cases Result err
    (Error (e) (Error (cons (apply sprintf args) e)))
    (Ok (v) (error "not an Error") ) ) )  ;; TODO: fix message



;; Put the message from an exception into an Error
(define (Error-ex ex . args)
  (apply Error-wrap (Error-fmt (get-condition-property ex 'exn 'message)) args) )

;; TODO: Create a Result-Error exception condition to make it easy to escape
;; TODO: from procedures such as map?


;; Include rest of the code -------------------------------------------------

;; Procedures for creating and matching routes
(include-relative "router.scm")

;; Route handling procedures
(include-relative "handlers.scm")

;; Procedures for creating Gopher menus
(include-relative "menu.scm")

;; Procedures for starting and stopping a Gopher server
(include-relative "server.scm")



;; Miscellaneous definitions ------------------------------------------------

;; A char set for trimming selectors
(define selector-trim-char-set
  (char-set-adjoin char-set:whitespace #\/) )

;; Trim beginning and end of selector to remove whitespace and
;; '/' characters
(define (trim-selector selector)
  (string-trim-both selector selector-trim-char-set) )

;; Similar to error but passes arguments after location to sprintf to form
;; error message
(define (error* location . args)
  (error location (apply sprintf args) ) )


;; Raise an exception and point to the previous exception message at the end
;; of the message after '->' to add context to an exception by making a chain
;; of exceptions.
;; TODO: Do we still need this?
(define (error-wrap ex location formatstring . args)
  (error* location "~? -> ~A"
                   formatstring
                   args
                   (get-condition-property ex 'exn 'message) ) )

)
