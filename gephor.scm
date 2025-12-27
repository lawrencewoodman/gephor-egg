;;; A Gopher server module for Chicken Scheme
;;;
;;;
;;; Copyright (C) 2024-2025 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; TODO: rename  exported functions to make consistent and more predictable?
(module gephor
  (connection-id
   start-server stop-server
   make-request request-selector request-client-address
   make-router router-add router-match
   menu-item menu-item-file menu-item-url
   menu-render
   selector->local-path
   serve-dir
   serve-file
   serve-path
   serve-url
   max-file-size
   server-hostname
   server-port
   trim-path-selector
   safe-path?
   read-file)


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
        (chicken process signal)
        (chicken sort)
        (chicken string)
        (chicken tcp)
        (chicken type)
        magic
        logfmt-logger
        srfi-1
        srfi-13
        srfi-14
        srfi-18)

;; Import notes -------------------------------------------------------------
;; srfi-1         - List procedures
;; srfi-13        - String library
;; srfi-14        - Character set library
;; srfi-18        - Multithreading support
;; magic          - Magic file type recognition
;; logfmt-logger  - Logger using logfmt



;; Connections --------------------------------------------------------------

;; Each connection receives it's own connection ID which is passed to the
;; handler using this parameter.  It shouldn't be updated outside of this
;; module.
(define connection-id (make-parameter #f))


;; Configuration ------------------------------------------------------------

(define server-hostname (make-parameter "localhost"))
(define server-port     (make-parameter 70))


;; The maximum size of a file that can be read and sent
;; Defaults to 50Mb
;; TODO: Is this name ok.  Should it be read/write as not just files sent
(define max-file-size (make-parameter 50000000))


;; Record types -------------------------------------------------------------

;; TODO: Consider using parameters to pass this information around
(define-record-type request
  (make-request selector client-address)
  request?
  (selector request-selector)
  (client-address request-client-address)
)



;; Include rest of the code -------------------------------------------------

;; Procedures for creating and matching routes
(include-relative "router.scm")

;; Procedures for creating Gopher menus
(include-relative "menu.scm")

;; Route handling procedures
(include-relative "handlers.scm")

;; Procedures for starting and stopping a Gopher server
(include-relative "server.scm")

;; Miscellaneous procedures
(include-relative "misc.scm")



)
