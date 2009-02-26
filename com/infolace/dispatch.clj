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

(def reader-macros
     {'quote (int \'), 'clojure.core/meta (int \^), 'clojure.core/deref (int \@), 
      'var "#'", })
(def pprint-simple-list (formatter "~:<~@{~w~^ ~_~}~:>"))
(defn pprint-list [writer alis]
  (if-let [macro-char (reader-macros (first alis))]
    (do
      (.write writer macro-char)
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

(defn- single-defn [writer alis])
(defn- multi-defn [writer alis])

;;; TODO: figure out how to support capturing metadata in defns (we might need a 
;;; special reader
(defn pprint-defn [writer alis]
  (let [[defn-sym defn-name & stuff] alis
	[doc-str stuff] (if (string? (first stuff))
			  [(first stuff) (rest stuff)]
			  [nil stuff])]
    (pprint-logical-block [writer writer] alis :prefix "(" :suffix ")"
      (cl-format writer "~w ~1I~@_~w" defn-sym defn-name)
      (if doc-str (cl-format writer " ~_~w" doc-str))
      (cond
       (vector? (first stuff)) (single-defn writer stuff)
       (list? (first stuff)) (multi-defn writer stuff)
       :else (pprint-list writer stuff)))))

(def code-table { 'defn pprint-defn, })
(defn pprint-code-list [writer alis]
  )

;;; For testing
(comment

(pprint-defn *out* '(defn cl-format 
  "An implementation of a Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	navigator (init-navigator args)]
    (execute-format stream compiled-format navigator))))

)
nil

