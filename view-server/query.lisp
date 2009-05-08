(in-package :cl-couchdb-view-server)

(export '(query query-view query-view*))

;;; Interface for querying views

(defun prepare-keys (keys)
  "Prepare a plist to be send to server as query parameters \(put
strings in quotes and escape them)."
  (mapcar (lambda (_) (if (keywordp _) _ (json _))) keys))

(defun query-wrapper (res)
  "Take the result of the query and return as values the rows, the
number of total tows and the offset."
  (values (@ res :rows) (@ res :total-rows) (@ res :offset)))

(defun query (database map-reduce &rest key-args 
		 &key key startkey endkey count skip startkey_docid update descending group group_level)
  "Query an ad hoc view. `database' should match \(database &optional
\(server *couchdb-server*)) or be just the database name, `map-reduce'
should either be just the name of the map function or match \(map &key reduce)."
  (declare (ignore key startkey endkey count skip startkey_docid update descending group group_level))
  (destructuring-bind (map &key reduce) 
      (if (eq (car map-reduce) 'lambda)
	  (list map-reduce)
	  map-reduce)
    (destructuring-bind (database &optional (server *couchdb-server*))
	(ensure-list database)
     (let ((view (make-instance 'view)))
	(setf (map-of view)
	      (format-from-wild `#',map))
	(when reduce
	  (setf (reduce-of view)
		(format-from-wild `#',reduce)))
	(query-wrapper 
	 (couch-request* :post server `(,database _temp_view) 
	     (prepare-keys  key-args)
	     +json-content-type+
	     view))))))

(defun query-view (view &rest keys &key key startkey endkey count skip startkey_docid update descending group group_level)
  "Query `view'. `view' may be either a symbol naming a view (the last
server and database the view has been synced to will be used in this
case) or match the lambda-list (view &optional (db default-db) (server
*couchdb-server*)), where `default-db' is like in the former case."
  (declare (ignore key startkey endkey count skip startkey_docid update descending group group_level))
  (destructuring-bind (default-db design-doc)
      (car (gethash view *views*))
    (destructuring-bind (view &optional (db default-db) (server *couchdb-server*))
	(ensure-list view)
      (query-wrapper (couch-request* :get server (list db '_view design-doc view) (prepare-keys keys))))))

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