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

(dosync (ref-set *print-pprint-dispatch* []))

(def pprint-list (formatter "~:<~@{~w~^ ~_~}~:>"))
(dosync (alter *print-pprint-dispatch* conj [list? pprint-list]))
(dosync (alter *print-pprint-dispatch* conj [#(instance? clojure.lang.LazyCons %) pprint-list]))

(def pprint-vector (formatter "~<[~;~@{~w~^ ~_~}~;]~:>"))
(dosync (alter *print-pprint-dispatch* conj [vector? pprint-vector]))

(def pprint-map (formatter "~<{~;~@{~<~w~^, ~_~w~:>~^ ~_~}~;}~:>"))
(dosync (alter *print-pprint-dispatch* conj [map? pprint-map]))

(def pprint-set (formatter "~<#{~;~@{~w~^ ~:_~}~;}~:>"))
(dosync (alter *print-pprint-dispatch* conj [set? pprint-set]))

(defn pprint-ref [writer ref]
  (pprint-logical-block [writer writer] ref
    (write "@" :stream writer)
    (write @ref :stream writer)))
(dosync (alter *print-pprint-dispatch* conj [#(instance? clojure.lang.Ref %) pprint-ref]))

nil
