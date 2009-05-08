(in-package :cl-couchdb-view-server)

(export '(view design-document find-design-document defview sync-design-document defdesign))

;;; Simple usage case:
;;
;; (in-document test)

;; (defview test (doc)
;;   (emit 1 doc))

;; (defview (test :reduce) (docs)
  
;;   docs)

;; (defview test2 (doc)
;;   (emit 2 doc))

;; (sync-design-document 'beverages 'test) ;beverages is the name of the db


(defparameter *design-document* nil
  "Special variable for the design-document.")

(defparameter *design-documents* (make-hash-table :test 'equal)
  "Names -> design-documents.")

(defparameter *views* (make-hash-table)
  "Hashtable from view-names to pairs \(db design-doc) they are synced
  with.")

(defclass* view ()
  ((name nil)
   (map nil)
   (reduce nil)
   (map-code nil 
	     :documentation "Text representation of the code of the
	     map function.")
   (reduce-code nil 
		:documentation "Text representation of the reduce
		function.")))

(defclass* design-document ()
  (name
   (revision nil)
   (package)
   (views (make-hash-table) 
	  :documentation "A hastable of views, indexed by their
	  names")
   (documentation nil :documentation "A slot for documentation regarding the view.")))

(defmethod print-object ((view view) s)
  (format s "#<view ~a :map ~s :reduce ~s>" (name-of view) (map-of view) (reduce-of view)))

(defmethod code-of ((view view) &optional stream)
  (declare (ignore stream))
  (list* (map-code-of view)
   (when (reduce-of view) 
     (reduce-code-of view))))

(defmethod code-of ((doc design-document) &optional stream)
  (format stream "~s"
	  `(let ((*package* (find-package ,(intern (package-name *package*) :keyword))))
	     ,@(iter (for (k v) in-hashtable (views-of  doc))
			  (appending (code-of v))))))

(defmethod print-object ((doc design-document) s)
  (format s "#<design-document :name ~a :revision ~a :views ~s>" (name-of doc) (revision-of doc) (iter (for (k v) in-hashtable (views-of doc))
				  (collect v))))

(defmethod json:encode-json ((doc design-document) s)
  (json:encode-json
   `((language . "common-lisp")
     (code . ,(code-of doc))
     ,@(when (documentation-of doc)
	     `((documentation . ,(documentation-of doc))))
     ,@(when (revision-of doc) 
	     `((_rev . ,(revision-of doc))))
     (views . (,@(iter (for (name view) in-hashtable (views-of doc))
		       (collect (cons (couch-urlize name) view)))))) s))

(defmethod json:encode-json ((view view) s)
  (json:encode-json
   (apply 'plist-alist* :language "common-lisp" :map (map-of view) (when (reduce-of view) (list 'reduce (reduce-of view)))) s))

(defun ensure-view (view-name &optional (design-document *design-document*))
  (or (gethash view-name (views-of design-document))
      (setf (gethash view-name (views-of design-document))
	    (make-instance 'view :name view-name))))

(defun find-design-document (name)
  (nth-value 0 (gethash name *design-documents*)))

(defmethod (setf map-or-reduce-code-of) (val key (view view))
  (ecase key
    (:map (setf (map-code-of view) val))
    (:reduce (setf (reduce-code-of view) val))))

(defmethod map-or-reduce-code-of (key (view view))
  (ecase key
    (:map (map-code-of view))
    (:reduce (reduce-code-of view))))

(defmethod (setf map-or-reduce-of) (val key (view view))
  (ecase key
    (:map (setf (map-of view) val))
    (:reduce (setf (reduce-of view) val))))

(defmethod map-or-reduce-of (key (view view))
  (ecase key
    (:map  (map-of view) )
    (:reduce  (reduce-of view))))

(let ((package (make-package (gensym))))
 (defun format-from-wild (thing &optional stream)
   "Format the readable representation of `thing' as if it was done
from package. This is useful if you have to read `thing' back in a
different package."
   (let ((*package* package))
     (format stream "~s ~s" thing (gensym))))) ;; this gensym is to force couchdb to reload the view

(defmacro define-view (name type args &rest body)
  (let* ((function-name (intern (format nil "~A-~A" name type)))
	 (function-definition
	 `(defun ,function-name ,args
	       ,@(subst function-name name body))))
    `(progn
      (let ((view (ensure-view ',name)))
	(setf (map-or-reduce-of ,type view)
	      (format-from-wild '#',function-name))
	(setf (map-or-reduce-code-of ,type view)
	      ',function-definition))
      
      ,function-definition)))

(defmacro defdesign (name functions &body options)
  "Define a design document. `Functions' are lists that match the
lambda-list (type name args &body body), with type being one of `:map'
or `:reduce'.

Options are lists whose car is the name of the option and cdr the
option parameters. There may be many instances of options with the
same name.

Available options:

 `:documentation' A string documenting the view that gets sent to the
 server.

 `:sync' Put this design document on CouchDB. Arguments: (db
 &optional (server *couchdb-server*)). There may be more than one
 `sync' clause in the definition of the design document.
"
  (let ((function-definitions 
	 (iter (for function in functions)
	       (collect `(define-view ,@function)))))
    `(let* ((*design-document* 
	     (make-instance 'design-document :name ',name :package *package*)))
       (progn 
	 ,@function-definitions
	 ,@(iter (for option in options) 
	     (collect (ecase (car option)
			(:documentation `(setf (documentation-of *design-document*)
					       ,(cadr option)))
			(:sync `(apply #'sync-design-document *design-document* ',(cdr option))))))
	 (setf (gethash ',name *design-documents*) *design-document*)))))



(defmethod sync-view ((view view) (doc design-document) (db symbol))
  (pushnew (list db (name-of doc)) (gethash (name-of view) *views*) :test 'equal))

(defmethod sync-view ((view view) (doc symbol) (db symbol))
  (sync-view view (find-design-document doc) db))

(defmethod sync-view ((view view) (doc null) (db symbol))
  (error "There's no such design-document."))

(defmethod sync-design-document ((doc design-document) database &optional (couchdb-server *couchdb-server*))
  (let* (req
	 (uri-spec 
	  (list database  
		(couch-urlize (format nil "_design/~a" (couch-urlize (name-of doc))))))
	 (syncing (lambda ()
		    (couch-request* :put couchdb-server 
			uri-spec
			()
			+json-content-type+
			doc))))
    (handler-case 
	(setf req (funcall syncing))
      (couchdb-conflict (con)
	;; when the code of the conflicting view is identical, we can
	;; safely assume that the view has already been put into the
	;; database and we needn't do anything except setting the _rev
	;; of the design-doc.
	(let* ((old-doc (old-doc-of con))
	       (old-rev (@ old-doc :_rev)))
	  (if (string= (code-of doc) (@ old-doc :code))
	    (setf (revision-of doc) old-rev)
	    (progn 
	      (restart-case 
		  (setf req (funcall syncing))
		(proceed-anyway ()
		  :report "Put the document in the db anyway. The
		  revision of the document which causes a conflict
		  will be deleted. This is potentially dangerous---you
		  may overwrite the view in the database with data
		  that is out of date. At the least this will cause
		  the view to be reindexed."
		  (progn 
		    (couch-request* :delete couchdb-server uri-spec (list :rev old-rev))
		    (funcall syncing)))
		(do-nothing ()
		  :report "Just ignore it."))
	      (setf (revision-of doc) (@ req :rev)))))))
        
    (iter (for (name view) in-hashtable (views-of doc))
	  (sync-view view doc database))
    doc))

(defmethod sync-design-document (database (doc symbol) &optional (couchdb-server *couchdb-server*))
  (sync-design-document database (find-design-document doc) couchdb-server))


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