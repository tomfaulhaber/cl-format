;   Copyright (c) Tom Faulhaber, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.pprint
  (:use com.infolace.format.utilities)
  (:import [com.infolace.format PrettyWriter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that control the pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; *print-length*, *print-level* and *print-dup* are defined in clojure.core
;;; TODO: use *print-length*, *print-level* and *print-dup* here (or are they
;;; supplanted by other variables?)


(def
 #^{ :doc "Bind to true if you want write to use pretty printing"}
 *print-pretty* true)

;;; TODO: implement true data-driven dispatch
(def
 #^{ :doc "The pretty print dispatch table (N.B. This is not yet used)"}
 *print-pprint-dispatch* nil)

(def
 #^{ :doc "Pretty printing will try to avoid anything going beyond this column."}
 *print-right-margin* nil)

;;; TODO implement miser style
(def
 #^{ :doc "The column at which to enter miser style (N.B. This is not yet used)"}
 *print-miser-width* nil)

;;; TODO implement output limiting
(def
 #^{ :doc "Maximum number of lines to print in a pretty print instance (N.B. This is not yet used)"}
 *print-lines* nil)

;;; TODO: implement circle and shared
(def
 #^{ :doc "Mark circular structures (N.B. This is not yet used)"}
 *print-circle* nil)

;;; TODO: should we just use *print-dup* here?
(def
 #^{ :doc "Mark repeated structures rather than repeat them (N.B. This is not yet used)"}
 *print-shared* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the write function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def write-option-table
        {;:array            *print-array*
         ;:base             *print-base*,
         ;:case             *print-case*,
	 :circle           'com.infolace.pprint/*print-circle*,
         ;:escape           *print-escape*,
         ;:gensym           *print-gensym*,
	 :length           'clojure.core/*print-length*,
	 :level            'clojure.core/*print-level*,
	 :lines            'com.infolace.pprint/*print-lines*,
	 :miser-width      'com.infolace.pprint/*print-miser-width*,
	 :pprint-dispatch  'com.infolace.pprint/*print-pprint-dispatch*,
	 :pretty           'com.infolace.pprint/*print-pretty*,
         ;:radix            *print-radix*,
	 :readably         'clojure.core/*print-readably*,
	 :right-margin     'com.infolace.pprint/*print-right-margin*})

(defn- proc-write-options [options]
  (merge 
   (into {} (for [[k v] write-option-table] [k (var-get (find-var v))]))
   options))

;; TODO: build a macro that only rebinds changed things (base it on the
;; implementation of "binding")
(defmacro binding-map [symbol-map options & body]
  (let [optsym (gensym "options-")]
    `(let [~optsym ~options]
       (binding [~@(mapcat 
		    (fn [[key sym]] `(~sym (if (contains? ~optsym ~key)
					     (~key ~optsym)
					     (var-get (find-var (quote ~sym))))))
		    (eval symbol-map))]
	 ~@body))))

;; (defmacro binding-map [symbol-map options & body]
;;   (let [real-map (eval symbol-map)]
;;     `(binding [~@(mapcat 
;; 		  (fn [[key val]] (if-let [var-name (key real-map)]
;; 				    `(~var-name ~val)))
;; 		  options)]
;;        ~@body)))

(defmacro with-pretty-writer [[write-sym base-writer] & body]
  `(let [new-writer# (not (instance? PrettyWriter ~base-writer))
	 ~write-sym (if new-writer#
		      (PrettyWriter. ~base-writer *print-right-margin*)
		      ~base-writer)]
     ~@body
     (if new-writer# (.flush ~write-sym))))

(defn write [object options]
  "Write an object subject to the current bindings of the printer control variables.
Use the options argument to override individual variables for this call (and any 
recursive calls)"
  (binding-map write-option-table options 
    (let [optval (if (contains? options :stream) 
		   (:stream options)
		   true) 
	  base-writer (condp = optval
			       nil (java.io.StringWriter.)
			       true *out*
			       optval)]
      (if *print-pretty*
	(with-pretty-writer [pretty-writer base-writer]
	  ;; TODO: the real work!
	  )
	(binding [*out* base-writer]
	  (print object)))
      (if (nil? optval) 
	(.toString base-writer)))))

(defmacro pprint-logical-block 
  ""
  [stream-sym arg-list options & body])
