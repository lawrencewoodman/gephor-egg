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
   make-request request-selector request-client-address
   make-router router-add router-match
   menu-item menu-item-file menu-item-url
   menu-render
   serve-url
   serve-path
   Result Result? Ok Error Error? Error-ex Error-fmt Error-wrap
   max-file-size
   server-hostname
   server-port)


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



;; Configuration ------------------------------------------------------------

(define server-hostname (make-parameter "localhost"))
(define server-port     (make-parameter 70))


;; The maximum size of a file that can be read and sent
;; Defaults to 50Mb
;; TODO: Is this name ok.  Should it be read/write as not just files sent
(define max-file-size (make-parameter 50000000))


;; Record types -------------------------------------------------------------


(define-record-type request
  (make-request selector client-address)
  request?
  (selector request-selector)
  (client-address request-client-address)
)



;; Include rest of the code -------------------------------------------------

;; Definitions for Result variant type
(include-relative "result.scm")

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


)
