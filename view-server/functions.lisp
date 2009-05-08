(in-package :cl-couchdb-view-server)

(export '(emit is logit))

(defvar *map-doc-results* nil)

(defvar *in-couch-server* nil)

(defvar *log-stream* nil)

;;; Functions to be used in views

(defun emit (key value)
  "Add the pair key, value to the results. If called outside a view
returns the list (key value)."
  (if *in-couch-server* 
      (push (list key value) *map-doc-results*)
      (list key value)))

(defun is (doc type)
  "Check the type of doc. This is function is useful to avoid
lisp->json->lisp translation confusions."
  (string= (symbol-name type) (@ doc :type)))

(defun logit (format-string &rest args)
  "Format `args' using `format-string' to CouchDB's log file. In the
JS view-server this function's name is `log'. If called outside a
view, print what would be sent to CouchDB to `*standard-output*'"
  (let ((stream (or *log-stream* t))
	(log-body (handler-case (apply #'format nil format-string args)
		    (error (err)
		      (format nil "~s" err)))))
    (json-stream (list (cons :log log-body)) stream)
    (terpri stream)
    (force-output stream)))



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

