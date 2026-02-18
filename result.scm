;;; Result variant type definitions
;;;
;;; The Result ADT is used to improve error handling by having a way
;;; to handle errors that we are expecting or can manage compared to
;;; exceptions which we aren't expecting.  This also makes testing
;;; easier.
;;;
;;; From this file the following are exported:
;;;   Result Result? Ok Not-Applicable Error
;;;
;;; Copyright (C) 2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

(define-datatype Result Result?
  (Ok (v any?))
  (Not-Applicable (x any?))      ; TODO: Can we omit argument?
  (Error (e string?) (le list-of-log-entry?) ) )

; TODO: Should we force Not-Applicable to only take #t ?

;; Internal Definitions ------------------------------------------------------

;; Used to allow Ok to contain anything
(define (any? x) #t)

(define (log-entry? e)
  (and (pair? e)
       (symbol? (car e) ) ) )

(define list-of-log-entry? (list-of? log-entry?))

