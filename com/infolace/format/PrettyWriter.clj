;   Copyright (c) Tom Faulhaber, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.PrettyWriter
  (:gen-class
   :extends com.infolace.format.ColumnWriter
   :init init
   :constructors {[java.io.Writer Integer] [java.io.Writer]}
   :methods [[startBlock [] void]
	     [endBlock [] void]
	     [newline [clojure.lang.Keyword] void]]
   :state state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros to simplify dealing with types and classes. These are
;;; really utilities, but I'm experimenting with them here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro #^{:private true} 
  getf 
  "Get the value of the field a named by the argument (which should be a keyword)."
  [sym]
  `(~sym @(.state ~'this)))

(defmacro #^{:private true} 
  setf [sym new-val] 
  "Set the value of the field SYM to NEW-VAL"
  `(alter (.state ~'this) assoc ~sym ~new-val))

(defmacro #^{:private true} deftype [type-name & fields]
  (let [name-str (name type-name)]
    `(do
       (defstruct ~type-name :type-tag ~@fields)
       (defn- ~(symbol (str "make-" name-str)) 
	 [& vals#] (apply struct ~type-name ~(keyword name-str) vals#))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The data structures used by PrettyWriter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct #^{:private true} logical-block :parent :section :indent)
(defstruct #^{:private true} section :parent)

(deftype buffer-blob :type :data)
(deftype nl :type :section)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the PrettyWriter instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -init 
  [writer max-columns] [[writer max-columns] 
			(ref {:logical-blocks (struct logical-block nil nil 0) 
			      :sections nil
			      :mode :writing})])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Write all the tokens that have been buffered
(defn- write-buffered-output [this])

;;; If there are newlines in the string, print the lines up until the last newline, 
;;; making the appropriate adjustments. Return the remainder of the string
(defn- write-initial-lines 
  [this s] 
  (let [lines (.split s "\n")]
    (if (= (count lines) 1)
      s
      (dosync 
       (let [base (getf :base)
	     prefix (:prefix (first (getf :logical-blocks)))] 
	 (if (= :buffering (getf :mode))
	   (write-buffered-output this))
	 (doseq [l (butlast lines)]
	   (.write base l)
	   (if prefix
	     (.write base prefix)))
	 (setf :buffering :writing)
	 (last lines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writer overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -write 
  ([this x]
     (condp 
      =	    ;TODO put these back up when the parser understands condp 
      (class x)

      String 
      (let [s #^String x
	    nl (.indexOf x (int \newline))]
	(dosync (if (neg? nl)
		  (setf :cur (+ (getf :cur) (count s)))
		  (setf :cur (- (count s) nl 1))))
	(.write (getf :base) s))

      Integer
      (let [c #^Character x]
	(dosync (if (= c (int \newline))
		  (setf :cur 0)
		  (setf :cur (inc (getf :cur)))))
	(.write (getf :base) c)))))

(defn- -flush [this]) ;; Currently a no-op

(defn- -close [this]) ;; Currently a no-op

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for PrettyWriter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -startBlock [] )
(defn- -endBlock [] )
(defn- -newline [type]
  )
