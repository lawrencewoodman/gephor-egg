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


;; TODO: Create a types file
(define-type handler ((struct request) -> string))
(define-type router (list-of (pair string handler)))


;; Exported Definitions ------------------------------------------------------

(: make-router ((list-of (pair string handler)) --> router))
(define (make-router . args)
  (for-each (lambda (r)
              (when (< 1 (count (lambda (x) (string=? (car x) (car r))) args))
                         (error* 'make-router "duplicate pattern: ~A" (car r))))
            args)
  (router-sort-routes args))


;; TODO: error if multiple * or * not at end
(: router-add (router string handler --> router))
(define (router-add router pattern proc)
  (if (assoc pattern router)
      (error* 'router-add "pattern already exists in router: ~A" pattern)
      (let ((route (cons pattern proc)))
        ; Add the route to the list of routes and resort
        (router-sort-routes (cons route router) ) ) ) )


;; TODO: should this return the pattern as well, perhaps using values?
(: router-match (router string --> (or handler boolean)))
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

;; Sort routes in descending order of length of string before '*'
;; so that most general matches are at the end of the list.  If two
;; patterns are the same length sort in lexical order
;; TODO: rewrite explanation as it isn't quite right
(: router-sort-routes (router --> router))
(define (router-sort-routes router)
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

