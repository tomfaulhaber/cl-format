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

(def *simple-dispatch* (ref []))
(def *code-dispatch* (ref []))

(def reader-macros {'quote \', 'clojure.core/meta \^, 'clojure.core/deref \@,  })
(def pprint-simple-list (formatter "~:<~@{~w~^ ~_~}~:>"))
(defn pprint-list [writer alis]
  (if-let [macro-char (reader-macros (first alis))]
    (do
      (.write writer (int macro-char))
      (write (frest alis) :stream writer))
    (pprint-simple-list writer alis)))
(dosync (alter *simple-dispatch* conj [list? pprint-list]))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.LazyCons %) pprint-list]))

(def pprint-vector (formatter "~<[~;~@{~w~^ ~_~}~;]~:>"))
(dosync (alter *simple-dispatch* conj [vector? pprint-vector]))

(def pprint-map (formatter "~<{~;~@{~<~w~^ ~_~w~:>~^, ~_~}~;}~:>"))
(dosync (alter *simple-dispatch* conj [map? pprint-map]))

(def pprint-set (formatter "~<#{~;~@{~w~^ ~:_~}~;}~:>"))
(dosync (alter *simple-dispatch* conj [set? pprint-set]))

(defn pprint-ref [writer ref]
  (pprint-logical-block [writer writer] ref :prefix "#<Ref " :suffix ">"
    (write @ref :stream writer)))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Ref %) pprint-ref]))

(defn pprint-atom [writer ref]
  (pprint-logical-block [writer writer] ref :prefix "#<Atom " :suffix ">"
    (write @ref :stream writer)))
(dosync 
 (alter 
  *simple-dispatch* conj
  [#(instance? clojure.proxy.java.util.concurrent.atomic.AtomicReference$IRef %)
   pprint-atom]))

(defn pprint-agent [writer ref]
  (pprint-logical-block [writer writer] ref :prefix "#<Agent " :suffix ">"
    (write @ref :stream writer)))
(dosync (alter *simple-dispatch* conj [#(instance? clojure.lang.Agent %) pprint-agent]))

(dosync (ref-set *print-pprint-dispatch* @*simple-dispatch*))
nil

