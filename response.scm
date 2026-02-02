;;; Procedures for sendng a response to the client
;;;
;;; Definitions are exported in gephor.scm
;;; From this file the following are exported:
;;;   send-response send-response/error-menu
;;;
;;; Copyright (C) 2026 Lawrence Woodman <https://lawrencewoodman.github.io/>
;;;
;;; Licensed under an MIT licence.  Please see LICENCE.md for details.
;;;


;; Exported Definitions ------------------------------------------------------

;; Send a response back to the client.  The size of the response must be no
;; more than parameter: max-response-size
;; If the response it too big, it logs an error and sends an error
;; menu.
;; TODO: is sending an error menu appropriate when sending a file?
;; Returns #f if response is too big, otherwise #t
(: send-response (string output-port) -> bool)
(define (send-response response out)
  (if (> (string-length response) (max-response-size))
      (begin
        (apply log-error
               "response is too big to send"
               (log-context))
        (send-response/error-menu "resource unavailable" out)
        #f)
      (begin
        (write-string response #f out)
        #t) ) )


;; Send an error menu as a response with 'msg' as the username.
;; The selector isn't included in the error menu item in case that
;; could lead to an attack on the client.
;; TODO: limit the write-string size instead of #f
(: send-response/error-menu (string output-port) -> undefined)
(define (send-response/error-menu msg out)
  (let ((item (menu-item 'error msg "" (server-hostname) (server-port))))
    (write-string (menu-render (list item)) #f out) ) )


;; TODO: create a send-response/menu or send/menu
;; TODO: create a send-response/file or send/file

