(in-package :cl-couchdb-client)

(export '(couchdb-condition couchdb-server-error couchdb-conflict couchdb-not-found old-doc-of new-doc-of))

(defcondition* couchdb-condition ()
  (number
   error
   reason)
  (:report (lambda (con s)
	     (format s "CouchDB returned an error: ~a (~a). Reason: ~a." (error-of con) (number-of con) (reason-of con)))))

(defcondition* couchdb-not-found (couchdb-condition)
  ()
  (:documentation "404"))

(defcondition* couchdb-conflict (couchdb-condition)
  ((old-doc nil)
   (new-doc nil)
   server
   db
   (number 412))
  
  (:documentation "412")
  (:report (lambda (con s)
	     (format s "CouchDB returned an error: ~a (~a). Reason: ~a in \"http://~a/~a\".~%~%Document  in database~%~%~s~%~%Conflicts with:~%~s" 
		     (error-of con) 
		     (number-of con)
		     (reason-of con)
		     (server-of con)
		     (db-of con)
		     (old-doc-of con)
		     (new-doc-of con)))))

(defcondition* couchdb-server-error (couchdb-condition)
  ()
  (:documentation "50x errors."))

;; Copyright (C) 2008
;; Ryszard Szopa <ryszard.szopa@gmail.com> 

;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any
;; damages arising from the use of this software.

;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:

;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.

;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.

;; 3. This notice may not be removed or altered from any source
;;    distribution.