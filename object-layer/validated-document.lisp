(in-package :cl-couchdb-object-layer)

(export '(validated-doc invalid-document constraints-of validators-of constraint-failed validates defdoc make-doc make-doc* make-doc-and-save make-doc-and-save* bless-doc))


(defclass* validated-doc (doc)
  ()
  (:documentation "A more sophisticated version of
    `document'. Allows to define validators and initforms for document
    attributes."))

(defcondition* invalid-document ()
  (doc))

(defcondition* validator-failed (invalid-document)
  (key validator value)
  (:report (lambda (con s)
	     (with-slots (doc key value validator) con
	      (format s "The document ~s is invalid. Reason: attribute
		      ~s with value ~s didn't validate using ~s" 
		      doc key value validator)))))

(defcondition* constraint-failed (invalid-document)
  (constraint)
  (:report (lambda (con s)
	     (with-slots (doc constraint) con
	      (format s "The document ~s is invalid. Reason: constraint ~s failed." 
		      doc constraint)))))

(defgeneric make-doc (cl &rest keys &key &allow-other-keys)
  (:documentation "Make a doc of class `cl'."))

(defmethod validates ((doc validated-doc) &optional (err-on-failure t))
  (iter (for (k . fun) in (validators-of doc))
	(always (or 
		 (handler-case (funcall fun (@ doc k))
		   (error ()))
		 (when err-on-failure
		   (error (make-condition 'validator-failed :doc doc :key k :value (@ doc k) :validator fun))))))
  (iter (for fun in (constraints-of doc))
	(always (or (handler-case (funcall fun doc)
		      (error ()))
		    (when err-on-failure
		      (error (make-condition 'constraint-failed :doc doc  :constraint fun)))))))

(defmacro defdoc (name attribute-defs &body options)
  "Define a document class with some attributes and the method `make-doc'
  to create objects of this class.

  Each attribute-definition should match the lambda list (name &key
  validator initform), where:

  `name' is a keyword (and not `:type')

  `validator' is a function taking one argument (the value of the
  attribute) which should fail if it is invalid

  `initform' which is either some lisp value or a function taking one
  argument (the document itself). If it is a fucntion, it will be
  called on the document if no value is provided to `make-doc'. These
  functions are called after both any arguments provided to `make-doc'
  have been assigned and initforms which were normal values were
  assigned. There are no guarantees about the order these functions
  are called.

  Available options:

  `:documentation' a docstring
 
  `default-db' default database objects of this class should be saved to

  `constraints' list of functions taking one argument (a document)
  returning true if the document is valid.
"
  (destructuring-bind (validators initforms constructors)
      (iterate (for pseudo-slot in attribute-defs)
	       (destructuring-bind (k &key validator initform documentation)
		   pseudo-slot
		 (declare (ignore documentation))
		 (assert (not (eq :type k)))
		 (when validator
		   (collect `(cons ,k ,validator) into validators))
		 (when initform
		   (if (and (consp initform) (or (eq (car initform) 'function) (eq (car initform) 'lambda)))
		       (collect (cons k initform) into constructors)
		       (appending (list k initform) into initforms))))
	       (finally (return (list validators initforms constructors))))
    
    `(progn
       (let ((class (defclass ,name (validated-doc)
		      ()
		      ,@(when (@ options :documentation)
			      (list (assoc :documentation options))))))

	 (defmethod validators-of ((doc ,name))
	   "Alist indexed by attribute names (keywords), whose values
           are one argument functions. An object is valid if every
           validator called on its attribute returns true. Each
           attribute can have many validators."
	   (list ,@validators))

	 (defmethod constraints-of ((doc ,name))
	   (list ,@(@ options :constraints)))

	 (defmethod default-db-of ((doc ,name))
	   "Default db to which the document should be saved."
           ,(car (@ options :default-db)))
	 
	 class)
       (defmethod make-doc* ((cl (eql ',name)) attributes)
	 (apply 'make-doc cl (alist-plist attributes)))
       (defmethod make-doc ((cl (eql ',name)) &rest keys &key &allow-other-keys)
	 (let ((doc			; set default values
		(make-instance ',name :attributes (acons :type ,(format nil "~a::~a" (package-name *package*) (symbol-name name)) (plist-alist (list ,@initforms))))))
	   (iter (for key in keys by #'cddr)
		 (for value in (cdr keys) by #'cddr)
		 (setf (@ doc key) value))
	   ,@(iter (for (k . constructor) in constructors)
		   (collect `(unless (getf keys ,k) 
			       (setf (@ doc ,k) (funcall ,constructor doc)))))
	   doc)))))

;; fix the server thing
(defmethod save-doc ((doc validated-doc) &optional db  server)
  (declare (ignore server))
  (cond (db (call-next-method))
	((default-db-of doc)
	 (validates doc)
	 (save-doc doc (default-db-of doc)))))

(defmethod make-doc-and-save* (db type &rest args)
  (let ((doc (apply #'make-doc type args)))
    (save-doc doc db)))

(defmethod make-doc-and-save (type &rest args)
  (let ((doc (apply #'make-doc type args)))
    (save-doc doc)))

(defgeneric bless-doc (alist/doc &optional class)
  (:documentation "Transform an `alist/doc' or `doc' into a
  validated-doc, whose class is determined by `class' or taken from
  the `:type' attribute of `alist/doc'. This function is designed to
  be applied on the results of a CouchDB query."))

(defmethod bless-doc (alist &optional type)
  (apply 'make-doc (or type (read-from-string (@ alist :type))) (alist-plist alist)))

(defmethod bless-doc ((doc doc) &optional type)
  (change-class doc (or type (read-from-string (@ doc :type)))))

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