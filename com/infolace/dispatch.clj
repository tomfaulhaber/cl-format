;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.dispatch
  (:use com.infolace.format.utilities
	com.infolace.format
	com.infolace.pprint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations of specific dispatch table entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO replace direct stream calls with formatter funcs, when possible,
;; pprint macros otherwise
(defn pprint-list [writer lis colon? at-sign?]
  (pprint-logical-block [writer writer] lis :prefix "(" :suffix ")"
    (if (seq lis) 
      (loop [r lis]
	;;  (prlabel ppli r)
	(write (first r) :stream writer)
	(if-let [r (rest r)] 
	  (do
	    (.write writer " ")
	    (pprint-newline :linear writer)
	    (recur r)))))))

(dosync (alter *print-pprint-dispatch* conj [list? pprint-list]))
(dosync (alter *print-pprint-dispatch* conj [#(instance? clojure.lang.LazyCons %) pprint-list]))

(defn pprint-vector [writer avec colon? at-sign?]
  (do
    (.startBlock writer "[" nil "]")
    (if (seq avec) 
      (loop [r avec]
	;;  (prlabel ppli r)
	(write (first r) :stream writer)
	(if-let [r (rest r)] 
	  (do
	    (.write writer " ")
	    (.newline writer :linear)
	    (recur r)))))
    (.endBlock writer)))

(dosync (alter *print-pprint-dispatch* conj [vector? pprint-vector]))

(defn pprint-map [writer amap colon? at-sign?]
  (do
    (.startBlock writer "{" nil "}")
    (if (seq amap) 
      (loop [r amap]
	;;  (prlabel ppli r)
	(.startBlock writer nil nil nil)
	(let [[k v] (first r)] 
	  (write k :stream writer)
	  (.write writer " ")
	  (.newline writer :linear)
	  (write v :stream writer))
	(.endBlock writer)
	(if-let [r (rest r)] 
	  (do
	    (.write writer ", ")
	    (.newline writer :linear)
	    (recur r)))))
    (.endBlock writer)))

(dosync (alter *print-pprint-dispatch* conj [map? pprint-map]))

nil
