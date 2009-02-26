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

;;; TODO: optimize/compile format strings in dispatch funcs

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

;;; Format a defn with a single arity
(defn- single-defn [writer alis has-doc-str?]
  (if (seq alis)
    (do
      (if has-doc-str?
	(cl-format writer " ~_")
	(cl-format writer " ~@_"))
      (cl-format writer "~{~w~^ ~_~}" alis))))

;;; Format a defn with multiple arities
(defn- multi-defn [writer alis has-doc-str?]
  (if (seq alis)
    (cl-format writer " ~_~{~w~^ ~_~}" alis)))

;;; TODO: figure out how to support capturing metadata in defns (we might need a 
;;; special reader)
(defn pprint-defn [writer alis]
  (let [[defn-sym defn-name & stuff] alis
	[doc-str stuff] (if (string? (first stuff))
			  [(first stuff) (rest stuff)]
			  [nil stuff])]
    (pprint-logical-block [writer writer] alis :prefix "(" :suffix ")"
      (cl-format writer "~w ~1I~@_~w" defn-sym defn-name)
      (if doc-str
	(cl-format writer " ~_~w" doc-str))
      (cond
       (vector? (first stuff)) (single-defn writer stuff doc-str)
       (list? (first stuff)) (multi-defn writer stuff doc-str)
       :else (pprint-list writer stuff)))))

(def pprint-simple-code-list (formatter "~:<~1I~@{~w~^ ~_~}~:>"))
(def code-table { 'defn pprint-defn, })
(defn pprint-code-list [writer alis]
  (if-let [special-form (code-table (first alis))]
    (special-form writer alis)
    (pprint-simple-code-list writer alis)))

;;; For testing
(comment

(pprint-code-list *out* '(defn cl-format 
  "An implementation of a Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	navigator (init-navigator args)]
    (execute-format stream compiled-format navigator))))

(pprint-defn *out* '(defn cl-format 
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	navigator (init-navigator args)]
    (execute-format stream compiled-format navigator))))

(pprint-defn *out* '(defn- -write 
  ([this x]
;;     (prlabel write x (getf :mode))
     (condp 
      =	    ;TODO put these back up when the parser understands condp 
      (class x)

      String 
      (let [s0 (write-initial-lines this x)
	    s (.replaceFirst s0 "\\s+$" "")
	    white-space (.substring s0 (count s))
	    mode (getf :mode)]
	(if (= mode :writing)
	  (dosync
	   (write-white-space this)
	   (.col-write this s)
	   (setf :trailing-white-space white-space))
	  (add-to-buffer this (make-buffer-blob s white-space))))

      Integer
      (let [c #^Character x]
	(if (= (getf :mode) :writing)
	  (do 
	    (write-white-space this)
	    (.col-write this x))
	  (if (= c (int \newline))
	    (write-initial-lines this "\n")
	    (add-to-buffer this (make-buffer-blob (str (char c)) nil)))))))))
)
nil

