(defpackage #:cl-couchdb-view-server
  (:use :cl :iterate :defclass-star :logv :cl-couchdb-client)
  (:nicknames :couchdb-server)
  (:documentation
   "CouchDB view-server. Put \"common-lisp=/usr/bin/socat -
   TCP4:localhost:5477\" in the \"[Couch Query Servers]\" section of
   couchdb.ini. You must have socat installed on your system for this
   to work."))


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