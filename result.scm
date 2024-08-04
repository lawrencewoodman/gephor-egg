;;; Result variant type definitions
;;;
;;; The Result ADT is used to improve error handling by having a way
;;; to handle errors that we are expecting or can manage compared to
;;; exceptions which we aren't expecting.  This also makes testing
;;; easier.
;;;
;;; From this file the following are exported:
;;;   Result Result? Ok Error Error-ex Error-fmt Error-wrap
;;;
;;; TODO: Should Ok and Error be exported as Result-Ok, Result-Error?
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

(define-datatype Result Result?
  (Ok (v any?))
  (Error (e list-of-string?) ) )


; Pass the arguments to sprintf to create the error string making it easy to
; create a formatted erro string
(define (Error-fmt . args)
  (Error (list (apply sprintf args) ) ) )


(define (Error-wrap err . args)
  (cases Result err
    (Error (e) (Error (cons (apply sprintf args) e)))
    (Ok (v) (error "bad argument type") ) ) )


;; Put the message from an exception into an Error
(define (Error-ex ex . args)
  (apply Error-wrap (Error (list (get-condition-property ex 'exn 'message))) args) )

;; TODO: Create a Result-Error exception condition to make it easy to escape
;; TODO: from procedures such as map?



;; Internal Definitions ------------------------------------------------------

;; Used to allow Ok to contain anything
(define (any? x)
  #t)

(define list-of-string? (list-of? string?))

