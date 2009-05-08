(in-package :cl-couchdb-object-layer)

(export '(not-null between))

(defun not-null (x)
  (not (null x)))

(defun between (x y)
  (lambda (_)
    (<= x _ y)))

(defun emailp (x)
  "Chech whether x is a correct e-mail address. Differences from RFC 2822:

   - doesn't check whether there aren't 2 consecutive dots or a dot at
     the beginning or end of the local part.

   - checks the total length of the address, not the local and domain
     part separately.

   - doesn't allowed for adresses with quotation marks"
  (and (> (+ 64 255 1) (length x)) 
       (cl-ppcre:scan "^[A-Z0-9a-z_\\.!#\\$%\\*/\\?\\|\\^\\{\\}\\`\\~\&'\\+\\-=]+@([A-Z0-9a-z_\\-]+\\.)+[a-z]{2,3}$"
		      x)))

(defun emailp-test ()
  ;; cases taken from Wikipedia
  (iter 
    (for address in '("abc@example.com"
		      "Abc@example.com"
		      "aBC@example.com"
		      "abc.123@example.com"
		      "1234567890@example.com"
		      "_______@example.com"
		      "abc+mailbox/department=shipping@example.com"
		      "!#$%&'*+-/=?^_`.{|}~@example.com"))
    (if (emailp address)
	(format t ".")
	(format t "!")))
  (iter 
    (for address in '("Abc.example.com"
		      "Abc.@example.com"
		      "Abc..123@example.com"
		      "A@b@c@example.com"
		      "%()[]\\;:,<>@example.com"))
    (if (not (emailp address))
	(format t ".")
	(format t "!"))))


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