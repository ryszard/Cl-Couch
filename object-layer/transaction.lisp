(in-package :cl-couchdb-object-layer)

(export '(with-transaction save-doc delete-doc save-doc-list-with-example))

(defvar *in-transaction* nil)
(defvar *documents-in-transaction* nil)

(defmethod delete-doc :around ((doc doc) &optional server db)
  (declare (ignore server db))
  (if *in-transaction*
      (progn
	(when (not (@ doc :_id))
	  (error "You cannot delete a document that doesn't exist in the database: ~s" doc))
	(setf (@ doc :_deleted) t)
	(push doc *documents-in-transaction*))
      (call-next-method)))

(defmethod save-doc :around ((doc doc) &optional db server)
  (declare (ignore server db))
  (if *in-transaction*
      (push doc *documents-in-transaction*)
      (call-next-method)))

(defmethod save-doc ((null null) &optional db server)
  (declare (ignore null db server)))

(defmethod save-doc ((list list) &optional db (server *couchdb-server*))
  (save-doc-list-with-example (car list) db server))

(defgeneric save-doc-list-with-example (example list &optional db server)
  (:documentation "Save a list of docs, with example provided for
  class dispatching. This method is called by `save-doc' if called on
  a list."))

(defmethod save-doc-list-with-example ((doc doc) (list list) &optional db (server *couchdb-server*))
  (let* ((db (or db (default-db-of doc)))) 
    (iter (for doc in list)
	  (validates doc))
    (destructuring-bind (_ (_results . results)) 
	(couch-request*  :post server `(,db _bulk_docs) nil +json-content-type+ 
			 `((:hello . :kitty) (:docs . ,list))) 
					;the hello kitty thing is so
					;that cl-json knows it is an
					;object, not an array :/
      (declare (ignore _ _results))
      (iter (for res in results)
	    (for doc in list)
	    (setf (@ doc :_rev) (@ res :rev))))))

(defmacro with-transaction ((db &key (server *couchdb-server*)) &body body)
  `(let ((*in-transaction* t)
	 *documents-in-transaction*)
      ,@body
      (save-doc (nreverse *documents-in-transaction*) ',db ,server)))

;; example:
;; (with-transaction (test)
;;   (let ((doc1 (make-doc-instance :foo 1))
;; 	(doc2 (make-doc-instance :foo 2))
;; 	(doc3 (make-doc-instance :foo 3)))
;;     (save-doc doc1)
;;     (save-doc doc2)
;;     (save-doc doc3)

;;     (format t "~s ~s ~s~%" doc1 doc2 doc2)))


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