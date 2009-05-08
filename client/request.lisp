(in-package :cl-couchdb-client)

(export '(*couchdb-server* open-server couch-request* couch-request couch-urlize create-database +json-content-type+))

(unless (boundp '+json-content-type+) 
  (defconstant +json-content-type+ "application/json;charset=utf-8"))

(defvar *couchdb-server* nil
  "Default server used by requests \(string or uri). Initialized by `open-server'.")

(defun open-server (&optional (string-or-uri "http://localhost:5984"))
  "Open a CouchDB server."
  (defparameter *couchdb-server* string-or-uri))


;;; urlization

(defgeneric couch-urlize (t)
  (:documentation "Translate to a string representation appropriate to pass to a CouchDB url."))

(defmethod couch-urlize ((s (eql t)))
  "true")

(defmethod couch-urlize ((null null))
  "false")

(defmethod couch-urlize ((s symbol))
  (format nil (string-downcase (substitute #\_ #\- (symbol-name s)))))

(defmethod couch-urlize ((s string))
  (url-encode s))

(defmethod couch-urlize ((n number))
  n)

;;; requests

;;; this function is to make testing easier
(defun make-couch-uri (server path keys)
  (format nil "~a~{/~a~}~:[~;?~]~:*~{~a=~a~^&~}" server 
	  (mapcar 'couch-urlize path) 
	  (mapcar 'couch-urlize keys)))

(defun couch-request* (method server path &optional keys content-type content)
  "Make a request to `server'. `path' is a list of either strings
  or symbols. Key arguments map to GET parameters.

   The following key arguments are recognized: `key', `startkey',
  `endkey', `count', `skip', `startkey_docid', `update', `descending',
  `group', `group_level'."
  (let ((uri (make-couch-uri server (ensure-list path) keys)))
    (logv uri)
    (multiple-value-bind (responce error-number)
	(http-request method uri 
		      :content content ;(if (string= content-type +json-content-type+) (json content) content) 
		      :content-type content-type)
      (cond ((<= 200 error-number 299)
	     responce)
	    ((= error-number 412)
	     (error (apply 'make-condition 'couchdb-conflict 
			   :old-doc (handler-case (http-request :get uri)
				      (type-error () "unknown"))
			   :new-doc content
			   :db (car path)
			   :server server
			   (alist-plist responce))))
	    ((= error-number 404) 
	     (error (apply 'make-condition 'couchdb-not-found :number error-number (alist-plist responce))))
	    ((<= 500 error-number 599)
	     (error (apply 'make-condition 'couchdb-server-error :number error-number (alist-plist responce))))
	    (t 
	     (error (apply 'make-condition 'couchdb-condition :number error-number (alist-plist responce))))))))

(defmacro couch-request (req-spec (&rest path-and-keys) &optional content)
  "`req-spec' is either just the method, or matches the
  lambda-list (method &key (content-type \"application/json\") (server
  *couchdb-server*)). Path is a list of either strings or symbols,
  which at some point are key value pairs. These keys will map to GET
  parameters.

  For example, path equal to '(foo _all_docs :count 2 :startkey
  \"quux\") will map to \"/foo/_all_docs?count=2&startkey=%23quux%23\"


  The following key arguments are recognized: `key', `startkey',
  `endkey', `count', `skip', `startkey_docid', `update', `descending',
  `group', `group_level'."
  (multiple-value-bind (path keys)
      (iter (for el in path-and-keys)
	     (with keyword-seen = nil)
	     (when (keywordp el) (setf keyword-seen t))
	     (if keyword-seen (collect el into keys) (collect el into path))
	     (finally (return (values path keys))))
    (destructuring-bind (method &key (content-type +json-content-type+) (server *couchdb-server*))
	(ensure-list req-spec)
     `(couch-request* ,method ,server ',path ',keys ,content-type ,content))))

(defun create-database (name &optional (server *couchdb-server*))
  (couch-request* :put server (list name) () () ()))

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