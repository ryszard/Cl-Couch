(in-package :cl-couchdb-view-server)

(export '(*view-server* view-server start-view-server start-view-server* stop-view-server start stop))

(defparameter *view-server* nil)

(defvar *in-rereduce* nil)

(defvar *in-map* nil)

(defclass* view-server ()
  ((port 5477)
   (host "127.0.0.1")
   (thread))
  (:documentation "Class for the view server."))

(defmethod shared-initialize :after ((server view-server) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs slot-names))
  (start server))

(defmethod start ((server view-server))
  "Make the thread of `server' run."
  (handler-case (stop server)
    (unbound-slot ())
    #+SBCL (sb-thread:interrupt-thread-error ())
    #-SBCL (error ()))
  (setf (thread-of server)
	(make-server-thread (host-of server) (port-of server))))

(defmethod stop ((server view-server))
  "Make the thread of `server' stop."
  (bordeaux-threads:destroy-thread (thread-of server)))

(defmethod print-object ((server view-server) s)
  (format s "#<view-server :host ~s :port ~s>" (host-of server) (port-of server)))

(defun start-view-server (&rest args)
  "Start a view server and put it into `*view-server*'. Arguments are
passed to `view-server's `shared-initialize'."
  (when *view-server*
    (stop-view-server *view-server*))
  (setf *view-server* 
	(apply 'make-instance 'view-server args)))

(defun start-view-server* ()
  (start-view-server))

(defun stop-view-server (&optional (server *view-server* serverp))
  (stop server)
  (unless serverp
    (setf *view-server* nil)))

(defun server-repl (stream)
  (let (funs)
    (labels ((couch-funcall (fn x)
	       "Swallow any conditions and return json [] if no
                sensible output. Note: NIL is null not [] in cl-json,
                so we need a workaround to be able to have normal lisp
                functions \(not returning #()). Maybe null would also
                work?"
	       (let ((*in-couch-server* t)) 
		 (or (handler-case
			 (progn
			   (format-log "Calling function: ~a." fn)
			   (if *in-map*
			       (let (*map-doc-results*)
				 (funcall fn x)
				 (reverse *map-doc-results*))
			       (funcall fn x)))
		       (error (err)
			 (format-log "Function failed. Reason: ~a" (let (*print-escape*) (format nil "~a" err)))
			 nil)) 
		     #())))
	     (to-array (x)
	       (make-array 1 :initial-element x))
	     (reset ()
	       (setf funs nil)
	       (format-log "Resetting.")
	       t)
	     (add-fun (fun-string)
	       (handler-case 
		   (progn
		     (push (eval (read-from-string fun-string)) funs)
		     (format-log "Added function ~a." fun-string)
		     t)
		 (error (err)
		   (format-log "Error while trying to add fun: ~a: ~a" (type-of err) (let (*print-escape*) (format nil "~a" err)))
		   `((error . ((id . ,(type-of err)) (reason . ,(let (*print-escape*) (format nil "~a" err)))))))))
	     (map-doc (doc)
	       (format-log "Mapping document: ~a" doc)
	       (let ((*in-map* t))  
		 (mapcar (lambda (x) (couch-funcall x doc)) (reverse funs))))
	     (reduce* (fns keys-vals)
	       (format-log "Reducing keys-vals ~s" keys-vals)
	       (format-log "Using functions: ~a" fns)
	       (list t (to-array (mapcar (lambda (x) 
					   (couch-funcall (eval (read-from-string x)) (iter (for ((key id) val) in  keys-vals)
											    (collect (cons key val))))) fns))))
	     (rereduce (fns keys-vals)
	       (format-log "Rereducing keys-vals ~s" keys-vals)
	       (format-log "Using functions: ~a" fns)
	       (list t (to-array (mapcar (lambda (x) (let ((*in-rereduce* t)) (couch-funcall (eval (read-from-string x)) keys-vals))) fns))))) ;
					
      (handler-case 
	  (iter 
	    (let ((line (read-line stream)))
	      (format-log "CouchDB says: ~s" line)
	      (when (string= line "")
		(return))
	      (let ((*log-stream* stream))
		  (destructuring-bind (head &rest rest) (dejson line)
		    (json-stream (cond 
				   ((string= head "reset") (reset))
				   ((string= head "add_fun") (apply #'add-fun rest))
				   ((string= head "map_doc") (apply #'map-doc rest))
				   ((string= head "reduce") (apply #'reduce* rest))
				   ((string= head "rereduce") (apply #'rereduce rest))
				   (t (error "Unrecognized message: ~s ~s" head rest))) stream)
		    (terpri stream)
		    (force-output stream)))))
	(end-of-file ()
	  nil)))))


(defun make-server-thread (host port)
  "Start the server, run accept loop."
  (bordeaux-threads:make-thread 
   (lambda ()
     (let ((sock (usocket:socket-listen host port
					:reuseaddress t)))
       (labels ((accept-connection (s)
		  (let* ((lsock (usocket:socket-accept s))
			 (cstream (usocket:socket-stream lsock)))
		    (bordeaux-threads:make-thread
		     (lambda ()
		       (unwind-protect
			    (server-repl cstream)
			 (progn
			   (close cstream)
			   (usocket:socket-close lsock))))))
		  (accept-connection s)))
	 (unwind-protect
	      (accept-connection sock)
	   (usocket:socket-close sock)))))))


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