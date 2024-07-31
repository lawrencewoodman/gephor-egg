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
        magic
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
;; magic   - Magic file type recognition


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


;; Procedures for creating and matching routes
(include-relative "router.scm")

;; Route handling procedures
(include-relative "handlers.scm")

;; Procedures for creating Gopher menus
(include-relative "menu.scm")

;; Procedures for starting and stopping a Gopher server
(include-relative "server.scm")


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

)