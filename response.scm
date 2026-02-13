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

;; Send a response back to the client.
;;
;; If the response is bigger than the parameter, max-response-size,
;; it raises an error
(: send-response (string output-port --> undefined))
(define (send-response response out)
  (if (> (string-length response) (max-response-size))
      (error* 'send-response "response is too big to send")
      (write-string response #f out) ) )


;; Send an error menu as a response with 'msg' as the username.
;; The selector isn't included in the error menu item in case that
;; could lead to an attack on the client.
;; msg is truncated at 160 characters to prevent an attack on the client.
(: send-response/error-menu (string output-port --> undefined))
(define (send-response/error-menu msg out)
  (let ((item (menu-item 'error
                         (substring msg 0 (min 160 (string-length msg)))
                         ""
                         (server-hostname)
                         (server-port))))
    (write-string (menu-render (list item)) #f out) ) )

