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

;;; Handle forms that can be "back-translated" to reader macros
;;; Not all reader macros can be dealt with this way or at all. 
;;; Macros that we can't deal with at all are:
;;; ;  - The comment character is aborbed by the reader and never is part of the form
;;; `  - Is fully processed at read time into a lisp expression (which will contain concats
;;;      and regular quotes).
;;; ~@ - Also fully eaten by the processing of ` and can't be used outside.
;;; ,  - is whitespace and is lost (like all other whitespace). Formats can generate commas
;;;      where they deem them to help readability.
;;; #^ - Adding metadata completely disappears at read time and the data appears to be
;;;      completely lost.
;;;
;;; Most other syntax stuff is dealt with directly by the formats (like (), [], {}, and #{})
;;; or directly by printing the objects using Clojure's built-in print functions (like
;;; :keyword, \char, or ""). TODO: The notable exception is #() which is special-cased.

(def reader-macros
     {'quote (int \'), 'clojure.core/meta (int \^), 'clojure.core/deref (int \@), 
      'var "#'", 'clojure.core/unquote (int \~)})
(defn pprint-reader-macro [writer alis]
  (let [macro-char (reader-macros (first alis))]
    (if (and macro-char (= 2 (count alis)))
      (do
	(.write writer macro-char)
	(write (frest alis) :stream writer)
	true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch for the basic data types when interpreted
;; as data (as opposed to code).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pprint-simple-list (formatter "~:<~@{~w~^ ~_~}~:>"))
(defn pprint-list [writer alis]
  (if-not (pprint-reader-macro writer alis)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch for the code table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dosync (alter *code-dispatch* conj [vector? pprint-vector]))
(dosync (alter *code-dispatch* conj [map? pprint-map]))
(dosync (alter *code-dispatch* conj [set? pprint-set]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Ref %) pprint-ref]))
(dosync 
 (alter 
  *code-dispatch* conj
  [#(instance? clojure.proxy.java.util.concurrent.atomic.AtomicReference$IRef %)
   pprint-atom]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.Agent %) pprint-agent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something that looks like a defn or defmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Format the params and body of a defn with a single arity
(defn- single-defn [writer alis has-doc-str?]
  (if (seq alis)
    (do
      (if has-doc-str?
	(cl-format writer " ~_")
	(cl-format writer " ~@_"))
      (cl-format writer "~{~w~^ ~_~}" alis))))

;;; Format the param and body sublists of a defn with multiple arities
(defn- multi-defn [writer alis has-doc-str?]
  (if (seq alis)
    (cl-format writer " ~_~{~w~^ ~_~}" alis)))

;;; TODO: figure out how to support capturing metadata in defns (we might need a 
;;; special reader)
(defn pprint-defn [writer alis]
  (let [[defn-sym defn-name & stuff] alis
	[doc-str stuff] (if (string? (first stuff))
			  [(first stuff) (rest stuff)]
			  [nil stuff])
	[attr-str stuff] (if (string? (first stuff))
			   [(first stuff) (rest stuff)]
			   [nil stuff])]
    (pprint-logical-block [writer writer] alis :prefix "(" :suffix ")"
      (cl-format writer "~w ~1I~@_~w" defn-sym defn-name)
      (if doc-str
	(cl-format writer " ~_~w" doc-str))
      (if attr-str
	(cl-format writer " ~_~w" attr-str))
      (cond
       (vector? (first stuff)) (single-defn writer stuff (and doc-str attr-str))
       (list? (first stuff)) (multi-defn writer stuff (and doc-str attr-str))
       :else (pprint-list writer stuff)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format something with a binding form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: fix
(defn pprint-binding-form [writer binding-vec]
  (cl-format writer "~<[~;~@{~w~^ ~_~w~^, ~_~}~;]~:>" binding-vec))

(defn pprint-let [writer alis]
  (let [base-sym (first alis)]
    (pprint-logical-block [writer writer] alis :prefix "(" :suffix ")"
      (if (and (rest alis) (vector? (frest alis)))
	(do
	  (cl-format writer "~w ~1I~@_" base-sym)
	  (pprint-binding-form writer (frest alis))
	  (cl-format writer " ~_~{~w~^ ~_~}" (rrest alis)))
	(pprint-simple-code-list writer alis)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The master definitions for formatting lists in code (that is, (fn args...) or
;;; special forms).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pprint-simple-code-list (formatter "~:<~1I~@{~w~^ ~_~}~:>"))

(def code-table
     {'defn pprint-defn, 'defn- pprint-defn, 'defmacro pprint-defn,
      'let pprint-let,
      })

(defn pprint-code-list [writer alis]
  (if-not (pprint-reader-macro writer alis) 
	  (if-let [special-form (code-table (first alis))]
	    (special-form writer alis)
	    (pprint-simple-code-list writer alis))))
(dosync (alter *code-dispatch* conj [list? pprint-code-list]))
(dosync (alter *code-dispatch* conj [#(instance? clojure.lang.LazyCons %) pprint-list]))

(dosync (ref-set *print-pprint-dispatch* @*code-dispatch*))


;;; For testing
(comment

(pprint 
 '(defn cl-format 
    "An implementation of a Common Lisp compatible format function"
    [stream format-in & args]
    (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	  navigator (init-navigator args)]
      (execute-format stream compiled-format navigator))))

(pprint 
 '(defn cl-format 
    [stream format-in & args]
    (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	  navigator (init-navigator args)]
      (execute-format stream compiled-format navigator))))

(pprint
 '(defn- -write 
    ([this x]
       ;;     (prlabel write x (getf :mode))
       (condp 
	=   ;TODO put these back up when the parser understands condp 
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

