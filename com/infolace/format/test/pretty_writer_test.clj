;   Copyright (c) Tom Faulhaber, Feb 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.test.pretty-writer-test
  (:use unit-test com.infolace.format)
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
