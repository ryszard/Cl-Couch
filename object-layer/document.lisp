(in-package :cl-couchdb-object-layer)

(export '(@ doc aget id-of rev-of get-doc-from-id make-doc save-doc))

(defun make-alist ()
  '((nil)))

(defclass* doc ()
  ((attributes (make-alist) :documentation "Alist taken from couchdb")
   (original-db nil))
  (:documentation "A thin objectish wrapper over an alist returned by
  CouchDB."))

(defmethod print-object ((doc doc) s)
  (format s "#<doc(~s)~{ ~s~}>" (original-db-of doc) (alist-plist (attributes-of doc))))

(defmethod (setf aget) (val item (doc doc))
  (let ((atts (attributes-of doc)))
    (setf (aget item atts) val)
    (setf (attributes-of doc) atts)
    val))

(defmethod aget (item (doc doc))
  (aget item (attributes-of doc)))

(defmethod id-of ((doc doc))
  (@ doc :_id))

(defmethod (setf id-of) (val (doc doc))
  (setf (@ doc :_id) val))

(defmethod rev-of ((doc doc))
  (@ doc :_rev))

(defmethod (setf rev-of) (val (doc doc))
  (setf (@ doc :_rev) val))

(defun get-doc-from-id (id db &key (class 'doc) (server *couchdb-server* serverp))
  "Get the document whose id is `id' from database `db'."
  (make-instance class :attributes (couch-request* :get server (list db id)) :original-db (if serverp (list db server) db)))

(defmethod  make-doc ((n (eql nil)) &rest keys &key &allow-other-keys)
  "Make a document whose attributes are like the ones specified by `keys'"
  (make-instance 'doc :attributes (or (plist-alist keys) (make-alist))))

(defmethod json:encode-json ((doc doc) s)
  (json:encode-json (attributes-of doc) s))

(defmethod save-doc ((doc doc) &optional db (server *couchdb-server*))
  (destructuring-bind ((ok_ . ok) (id_ . id) (rev_ . rev))
      (let ((id (id-of doc)))
	(couch-request* (if id :put :post) server (list* (or db (original-db-of doc)) (when id
									    (list id))) 
	    () +json-content-type+ doc))
    (declare (ignore ok_ id_ rev_))
    (when ok
     (setf (id-of doc) id)
     (setf (rev-of doc) rev))))

(defmethod validates ((doc doc) &optional (err-on-failure t))
  (declare (ignore doc err-on-failure))
  t)

(defmethod default-db-of ((doc doc))
  (original-db-of doc))
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
