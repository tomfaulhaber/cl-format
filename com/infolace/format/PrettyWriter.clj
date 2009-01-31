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

(defstruct #^{:private true} logical-block :parent :section :indent)
(defstruct #^{:private true} section :parent)
(defstruct #^{:private true} buffer-blob :type :data)
(defstruct #^{:private true} nl :type)

(defn- -init 
  [writer max-columns] [[writer max-columns] 
			(ref {:logical-blocks nil 
			      :sections nil})])

(defmacro ; #^{:private true} 
  getf 
  "Get the value of the field a named by the argument (which should be a keyword)."
  [sym]
  `(~sym @(.state ~'this)))

(defmacro #^{:private true} 
  setf [sym new-val] 
  "Set the value of the field SYM to NEW-VAL"
  `(alter (.state ~'this) assoc ~sym ~new-val))

(defn- -write 
  ([this x]
     (condp 
      =	    ;TODO put these back up when the parser understands condp 
      (class x)

      String 
      (let [s #^String x
	    nl (.lastIndexOf x (int \newline))]
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

(defn- -startBlock [] )
(defn- -endBlock [] )
(defn- -addnewline [type])

(defn- -flush [this]) ;; Currently a no-op

(defn- -close [this]) ;; Currently a no-op
