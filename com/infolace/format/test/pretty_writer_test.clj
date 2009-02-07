;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.test.pretty-writer-test
  (:use unit-test 
	com.infolace.pprint
	com.infolace.format 
	com.infolace.format.utilities)
  (:import [com.infolace.format PrettyWriter]))

(defn test1 [] 
  (let [writer (PrettyWriter. *out* 15)]
    (.startBlock writer "(" nil ")")
    (.write writer "x ")
    (.newline writer :linear)
    (.write writer "y")
    (.endBlock writer)
    (.write writer "\n")
    (.close writer)))

(defn pp-list-internal [writer lis]
  (if (list? lis)
    (do
      (.startBlock writer "(" nil ")")
      (if (seq lis) 
	(loop [r lis]
;	  (prlabel ppli r)
	  (pp-list-internal writer (first r))
	  (if-let [r (rest r)] 
	    (do
	      (.write writer " ")
	      (.newline writer :linear)
	      (recur r)))))
       (.endBlock writer))
    (.write writer (print-str lis))))

(defn test2 [] 
  (let [writer (PrettyWriter. *out* 15)]
    (pp-list-internal writer '((x y) (z (a b c) d)))
    (.write writer "\n")
    (.close writer)))

(defn test3 [] 
  (with-open [writer (PrettyWriter. *out* 30)]
    (pp-list-internal writer '((x y) (z (a b c) d)))
    (.write writer "\n")))

(defn pp-list [lis max-col]
  (with-open [writer (PrettyWriter. *out* max-col)]
    (pp-list-internal writer lis)
    (.write writer "\n")))

(defn test4 [x] (pp-list '(a (b c)) x))
(defn test5 [x] (pp-list '((x y) (z (a b c) d)) x))

(defn write-margin [arg margin] 
  (apply write arg (if margin [:right-margin (first margin)] [])))

(defn test6 [& x] (write-margin '(a (b c)) x))
(defn test7 [& x] (write-margin '((x y) (z (a b c) d)) x))
(defn test8 [& x] (write-margin '((x y) [z (a b c) d]) x))
(defn test9 [& x] (write-margin '((x y) [z {:first a :second b :third c} d]) x))
(defn test10 [& x] 
  (write-margin 
   '(defmethod write-token :start-block [this token]
      (let [lb (:logical-block token)]
	(dosync
	 (prlabel write-start-block (:prefix (:logical-block token)))
	 (if-let [prefix (:prefix lb)] 
	   (.col-write this prefix))
	 (let [col (.getColumn this)]
	   (ref-set (:start-col lb) col)
	   (ref-set (:indent lb) col)))))
   x))
