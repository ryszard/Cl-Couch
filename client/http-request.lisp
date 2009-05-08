(in-package :cl-couchdb-client)

;;; Abstraction layer over the http-client library.

(setf drakma:*drakma-default-external-format* :utf-8)

(defun http-request (method uri &key content content-type)
  "Abstraction over a GET http request. Returns the dejsonified body
of the answer as the first value and the status code as the second
value."
  (multiple-value-bind (body status-code headers reply-uri stream closed-p reason) 
      ;;; this is like a big kludge. some day it should be fixed.
      (drakma:http-request 
       uri 
       :content 
       (when content
	 (lambda (s) 
	   (trivial-utf-8:write-utf-8-bytes (if (string= content-type +json-content-type+)
						(json content) 
						content) s)))
       :force-binary nil
       :content-type content-type
       :method method 
       :user-agent "cl-couchdb-client"
       :external-format-in :utf-8 :external-format-out :utf-8
       )
    (declare (ignorable headers reply-uri stream closed-p reason))
    (when (string= content-type +json-content-type+)
      (logv (json content)))
    (logv status-code body headers reply-uri stream closed-p reason)
    (values (dejson  body) status-code)))

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