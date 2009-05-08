(in-package :cl-couchdb-client)

(export '(json dejson dejson-stream json-stream))

;;; Abstraction layer over the json library

;;; Differences from cl-json in symbol<-> string translations:
;;; "this_is_a_lisp_symbol" -> :this-is-a-lisp-symbol
;;; "_id" -> :_id
;;; "_something_strange" -> :_something-strange

(setf json::*symbol-to-string-fn* (lambda (symbol)
				    (string-downcase (substitute #\_ #\- (symbol-name symbol)))))

(defun json::json-intern (string)
  (let ((symbol-name (substitute #\- #\_ (string-upcase (remove #\* (json::camel-case-to-lisp string))) :start 1)))
   (if json::*json-symbols-package*
       (intern symbol-name json::*json-symbols-package*)
       (intern symbol-name))))

(defun dejson (string)
  "Decode `string' from json."
  (json:decode-json-from-string string))

(defun json (object)
  "Encode `object' to json and return it as a string."
  (encode-json-to-string object))

(defun dejson-stream (&optional (stream *standard-input*))
  "Decode json from `stream'."
  (json:decode-json stream))

(defun json-stream (object &optional (stream *standard-output*))
  "Encode `object' to json and write it to `stream'."
  (json:encode-json object stream))

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
