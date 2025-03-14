;;; Procedures for creating and matching routes
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   make-router router-add router-match
;;;
;;; Copyright (C) 2024 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


(define-type handler ((struct request) -> string))
(define-type router (list-of (pair string handler)))


;; Exported Definitions ------------------------------------------------------

(: make-router ((list-of (pair string handler)) --> router))
(define (make-router . args)
  (for-each (lambda (r)
              (let ((r-pattern (car r)))
                (cond
                  ((< 1 (count (lambda (x) (string=? (car x) r-pattern)) args))
                    (error* 'make-router "duplicate pattern: ~A" r-pattern))
                  ((not (valid-pattern? r-pattern))
                    (error* 'make-router "invalid pattern: ~A" r-pattern)))))
            args)
  (sort-routes args) )


(: router-add (router string handler --> router))
(define (router-add router pattern proc)
  (cond
    ((assoc pattern router)
      (error* 'router-add "pattern already exists in router: ~A" pattern))
    ((not (valid-pattern? pattern))
      (error* 'router-add "invalid pattern: ~A" pattern))
    (else
      (let ((route (cons pattern proc)))
        ; Add the route to the list of routes and resort
        (sort-routes (cons route router) ) ) ) ) )


(: router-match (router string --> (or handler false)))
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
              (if (string=? selector pattern)
                  proc
                  (loop (cdr routes) ) ) ) ) ) ) )



;; Internal Definitions ------------------------------------------------------


(: valid-pattern? (string --> boolean))
(define (valid-pattern? pattern)
  (let ((splat-index (substring-index "*" pattern)))
    (or (not splat-index) (= splat-index (sub1 (string-length pattern) ) ) ) ) )


;; Sort routes in descending order of length of string before '*'
;; so that most general matches are at the end of the list.  If two
;; patterns are the same length sort in lexical order
;; TODO: rewrite explanation as it isn't quite right
(: sort-routes (router --> router))
(define (sort-routes router)
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

