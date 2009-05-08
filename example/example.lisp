(defpackage #:test-couchdb
  (:use :cl :couchdb-server :iterate :couchdb-client :warsaw-utils))

(in-package :test-couchdb)
(eval-when (:load-toplevel :execute)
 (open-server)
 (start-view-server*))

(let ((n 0))
  (defun make-blog-post (author title body &key for-real)
    (when for-real
      (incf n))
    (let ((post (plist-alist* :author author :body body :type 'blog-post :n n)))
      (if for-real
	  (couch-request* :put *couchdb-server* (list 'blog title) ()  +json-content-type+  post)
	  post))))

(let ((n 0))
  (defun make-blog-comment (post author body &key for-real)
    (when for-real
      (incf n))
    (let ((post (plist-alist* :author author :body body :post post :type 'comment :n n)))
      (if for-real
	  
	  (r (:post :content-type +json-content-type+) (blog) post)
	  post))))

(defdesign test
    ((by-author-type :map (doc)
       (emit (list (@ doc :author) (@ doc :type)) doc)))
  (:documentation "A test view.")
  (:sync blog))

; then run in the repl:
;; (create-database 'blog)
;; (iter (for title in '("first" "second" "third" "fourth"))
;;       (make-blog-post "foobar" title "Zażółć gęślą jaźń" :for-real t)
;;       (iter (for i from 1 to 4)
;; 	    (make-blog-comment title "baz" "Ala ma kota. äãẅ" :for-real t)))