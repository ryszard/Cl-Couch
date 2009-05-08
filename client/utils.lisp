(in-package :cl-couchdb-client)

(export '(alist-plist plist-alist ensure-list plist-alist* aget @ html-escape url-encode url-decode string-urlize))

(defun alist-plist (alist)
  "Transform an alist into a plist."
  (iter (for (k . v) in alist)
	(unless (and (null k) (null v))
	  (appending (list k v)))))

(defun plist-alist (plist)
  "Transform a plist into an alist."
  (iter (for k in plist by 'cddr)
	(for v in (cdr plist) by 'cddr)
	(collect (cons k v))))

(defun plist-alist* (&rest rest)
  (plist-alist rest))

(defgeneric aget (key alistish)
  (:documentation "Do a `key' lookup in `alistish' and return two
  values: the actual value and whether the lookup was successful (just
  like `gethash' does)."))

(defmethod aget (key (alist list))
  "The value of `key' in `alist'"
  (let ((x (assoc key alist :test #'equal)))
   (values 
    (cdr x)
    (consp x))))

(defmethod aget (key (hash hash-table))
  (gethash key hash))

(defmethod (setf aget) (value key (hash hash-table))
  (setf (gethash key hash) value))

(defmethod (setf aget) (value key (alist list))
  (if (null (assoc key alist))
      (progn
	(rplacd alist (copy-alist alist))
	(rplaca alist (cons key value))
	value)
      (cdr (rplacd (assoc key alist) value))))


(defun @ (alist key &rest more-keys)
  "Swiss army knife function for alists and any objects for whom
  `aget' has been defined. It works on alistish objects much much like
  the dot `.' works in many other object oriented
  languages (eg. Python, JavaScript). 

  (@ alistish-object :foo :bar :baz) is equivalent to
  calling (aget :baz (aget :bar (aget :foo x))), or
  alistish_object.foo.bar.baz in JS. A setter for `@' is also
  defined.
  
  It returns two values: the actual value and whether the lookup was
  successful (just like `gethash' does).

  `equal' is used for testing identity for keys."
  (if (null more-keys) 
      (aget key alist)
      (apply '@ (aget key alist) more-keys)))

(defun (setf @) (val alist key &rest more-keys)
  (if (null more-keys) 
      (setf (aget key alist) val)
      (setf (apply #'@ (aget key alist) more-keys) val)))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(proclaim '(inline ensure-list))

;;; The following functions have been stolen from Nick Allen's utils

(defmacro .do-array ((var array &optional return) &body body)
  "like DOLIST but for arrays"
  (let ((n% (gensym "array-index"))
	(a%  (gensym "array")))
    `(let ((,a% ,array))
       (declare (dynamic-extent ,a%))
       (dotimes (,n% (length ,a%) ,return)
	 (let ((,var (aref ,a% ,n%)))
	   ,@body)))))

(defmacro .do-string ((var string &optional return) &body body)
  "like DOLIST but for strings"
  `(.do-array (,var (the string ,string) ,return)
     (declare (type character ,var))
     ,@body))

(defun html-escape (string)
  (with-output-to-string (out)
  "returns an html-escaped version of `STRING'"
  ;; this function was created by modifying HUNCHENTOOT:ESCAPE-FOR-HTML
  (.do-string (c string)
    (case c
      ((#\<) (write-string "&lt;" out))
      ((#\>) (write-string "&gt;" out))
      ((#\") (write-string "&quot;" out))
      ((#\') (write-string "&#039;" out))
      ((#\&) (write-string "&amp;" out))
      (otherwise (write-char c out))))))

(defun url-encode (string)
  "returns a url-encoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  ;; this function was created by modifying HUNCHENTOOT:URL-ENCODE
  (with-output-to-string (out)
    (.do-string (c string)
      (if (or (char<= #\0 c #\9)
	      (char<= #\a c #\z)
	      (char<= #\A c #\Z)

	      ;; note that there's no comma in there - because of cookies

	      ;;       I don't know why there's no comma because of cookies, it's
	      ;;       what hunchentoot did so I copied it -nick

	      (find c "$-_.!*'()" :test #'char=))
	  (write-char c out)
	  (.do-array (b (trivial-utf-8:string-to-utf-8-bytes (string c)))
	    (format out "%~2,'0x" b))))))

(defun url-decode (string)
  "returns the a new url-decoded version of `STRING'. assumes UTF-8 so
be careful if seen around HUNCHENTOOT:URL-ENCODE, which probably
assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  ;; this function was created by modifying HUNCHENTOOT:URL-DECODE
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0))
	percent-p
	buff)
    (declare (dynamic-extent vector))
    (dotimes (n (length string) (trivial-utf-8:utf-8-bytes-to-string (make-array (length vector)
										 :element-type '(unsigned-byte 8)
										 :initial-contents vector)))
      (let ((c (char string n)))
	(cond (buff          (vector-push (parse-integer string
							 :start (1- n)
							 :end (1+ n)
							 :radix 16)
					  vector)
			     (setq buff nil))
	      (percent-p     (setq buff t
				   percent-p nil))
	      ((char= c #\%) (setq percent-p t))
	      (t             (vector-push (char-code (case c
						       ((#\+)     #\Space)
						       (otherwise c)))
					  vector)))))))

(defun string-urlize (string)
  "Make a string safe for an url:
    * Transform spaces to underscores
    * Remove unicode characters (for those for which there is a good
      latin-1 substitution, use it, otherwise insert #\_).
    * Decapitalize.
    * url-encode"
  (url-encode 
   (map 'string
        (lambda (x)
          (typecase x
            ((member #\Space #\Backspace #\Tab #\Rubout #\Linefeed
                     #\Return #\Page)
             #\_)
            (standard-char x)   ; FIXME: some non-alphabetic
                                ; characters still match
            ((eql #\ą) #\a)
            ((eql #\ć) #\c)
            ((eql #\ę) #\e)
            ((eql #\ł) #\l)
            ((eql #\ń) #\n)
            ((eql #\ó) #\o)
            ((eql #\ś) #\s)
            ((member #\ż #\ź) #\z)
            (t #\_)))
        (string-downcase string))))

;; Copyright (C) 2008
;; Ryszard Szopa <ryszard.szopa@gmail.com> & Nick Allen <nallen05@gmail.com>

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