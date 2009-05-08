; todo: -log rotation script
;       -emacs mode for viewing log files
;       -tests

(defpackage :logv
  (:use :cl)
  (:export :*log-output*
	   :*log-prefix-string*
	   :*log-thread-name-p*
	   :*log-date-p*
	   :*log-time-p*
           :write-string-to-log
           :format-log
	   :logv
	   :logvs))

(in-package :logv)

; configure these

(defvar *log-output* t
  "see WRITE-STRING-TO-LOG")

(defvar *log-prefix-string* "\
;"
  "the leftmost string prepended to the string sent to WRITE-STRING-TO-LOG-FILE by
FORMAT-LOG")

(defvar *log-thread-name-p* t
  "set to NIL if you don't want FORMAT-LOG to log the name of the current")

(defvar *log-date-p* t
  "set to NIL if you don't want FORMAT-LOG to log the current date")

(defvar *log-time-p* t
  "set to NIL if you don't want FORMAT-LOG to log the current time")

; special

(defvar *log-mutex%* (bordeaux-threads:make-lock "log stream")
  "prevents multiple threads from writing to *LOG-OUTPUT* at the same time")

; writing to the log file

(defun write-string-to-log (string)
  "if *LOG-OUTPUT* is

   -a stream: writes `STRING' to *LOG-OUTPUT*
   -T: writes `STRING' to *STANDARD-OUTPUT*
   -NIL: doesn't do anything
   -a pathname designator: writes `STRING' to the file designated by *LOG-OUTPUT*"
  (when *log-output*
    (bordeaux-threads:with-lock-held (*log-mutex%*)
      (if (or (stringp *log-output*)
	      (pathnamep *log-output*))
	  (with-open-file (out *log-output*
			       :element-type '(unsigned-byte 8)
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)
	    (trivial-utf-8:write-utf-8-bytes string out))
	  (write-string string (if (eql *log-output* t)
				   *standard-output*
				   *log-output*))))))

(defun .format-thread-name ()
  "writes the name of the thread like \"[thread-name] \" for FORMAT-LOG"
  (format nil "[~A] " (bordeaux-threads:thread-name (bordeaux-threads:current-thread))))

 (defun .format-log-date (ut)
  "writes the date like \"[DD:MM:YYY] \" for FORMAT-LOG"
   (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
     (declare (ignore sec min hour))
     (format nil "[~2,'0d:~2,'0d:~2,'0d] " day month year)))

 (defun .format-log-time (time)
   "writes the time like \"[HH:MM:SS] \" for FORMAT-LOG"
   (multiple-value-bind (sec min hour) (decode-universal-time time)
     (format nil "[~2,'0d:~2,'0d:~2,'0d] " hour min sec)))

(defun format-log (fmt-string &rest fmt-args)
  "calls WRITE-STRING-TO-LOG with the string created by combining `FMT-STRING' with
`FMT-ARGS' like FORMAT and prepending the result with all the necessary informative
garbage

the following code:

    \(format-log \"foo bar ~A\" 'baz\)

might result in the following line being sent to WRITE-STRING-TO-LOG:

   ;[repl-thread] [01:06:2008] [13:53:27] foo bar BAZ

see also: *LOG-PREFIX-STRING*, *LOG-THREAD-NAME-P*, *LOG-DATE-P*, and *LOG-TIME-P*"
  (when *log-output*
    (let ((ut (get-universal-time)))
      (write-string-to-log (concatenate 'string
					*log-prefix-string*
					(if *log-thread-name-p*
					    (.format-thread-name)
					    "")
					(if *log-date-p*
					    (.format-log-date ut)
					    "")
					(if *log-time-p*
					    (.format-log-time ut)
					    "")
					(apply #'format nil fmt-string fmt-args))))))

; logging arbitrary forms and their return values

(defmacro logv (form &rest more-forms)
  "behaves like PROGN except it logs its forms and their return values using
FORMAT-LOG. [and of course its forms are not reckognized by the compiler as being
top-level forms because it's not a special operator like PROGN]

the following code:

   (logv (+ 1 2) (+ 1 (logv (+ 1 1))))

will return 3 and probably result in something like the following lines being sent to
WRITE-STRING-TO-LOG:

   ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 2) -> 3
   ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 1) -> 2
   ;[repl-thread] [01:06:2008] [14:07:53] (+ 1 (LOGV (+ 1 1))) -> 3

see also: FORMAT-LOG, LOGVS"
  (if more-forms
      `(progn (logv ,form) (logv ,@more-forms))
      (let ((% (gensym "value")))
	`(let ((,% ,form))
	   (format-log "~S -> ~S" ',form ,%)
	   ,%))))

(defmacro logvs (form &rest more-forms)
  "lke LOGV but works with multiple return values"
  (if more-forms
      `(progn (logvs ,form) (logvs ,@more-forms))
      (let ((%l (gensym "values")))
	`(let ((,%l (multiple-value-list ,form)))
	   (format-log "~S -> values list: ~S" ',form ,%l)
	   (apply 'values ,%l)))))


;; Copyright (C) 2008
;; Nick Allen <nallen05@gmail.com> 

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