;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.test.pretty
  (:refer-clojure :exclude [format])
  (:use unit-test com.infolace.format com.infolace.pprint com.infolace.dispatch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unit tests for the pretty printer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simple-tests xp-fill-test
  (binding [*print-right-margin* 38]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
	       '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4) (*print-length* nil)\n      (z 2) (list nil))\n ...)\n"

  (binding [*print-right-margin* 22]
    (cl-format nil "(let ~:<~@{~:<~w ~_~w~:>~^ ~:_~}~:>~_ ...)~%"
	       '((x 4) (*print-length* nil) (z 2) (list nil))))
  "(let ((x 4)\n      (*print-length*\n       nil)\n      (z 2)\n      (list nil))\n ...)\n")

(simple-tests xp-miser-test
  (binding [*print-right-margin* 10, *print-miser-width* 9]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST\n first\n second\n third)"

  (binding [*print-right-margin* 10, *print-miser-width* 8]
    (cl-format nil "~:<LIST ~@_~W ~@_~W ~@_~W~:>" '(first second third)))
  "(LIST first second third)")


(simple-tests prefix-suffix-test
  (binding [*print-right-margin* 10, *print-miser-width* 10]
    (cl-format nil "~<{~;LIST ~@_~W ~@_~W ~@_~W~;}~:>" '(first second third)))
  "{LIST\n first\n second\n third}")

(simple-tests pprint-test
  (write '(defn foo [x y] 
	     (let [result (* x y)] 
	       (if (> result 400) 
		 (cl-format true "That number is too big")
		 (cl-format true "The  result of ~d x ~d is ~d" x y result))))
	 :stream nil)
  "(defn
 foo [x y]
 (let [result (* x y)]
  (if
   (> result 400)
   (cl-format true \"That number is too big\")
   (cl-format true \"The  result of ~d x ~d is ~d\" x y result))))")
