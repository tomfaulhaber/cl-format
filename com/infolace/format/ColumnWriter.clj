;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.ColumnWriter
  (:gen-class
   :extends java.io.Writer
   :init init
   :constructors {[java.io.Writer Integer] [], 
		  [java.io.Writer] []}
   :methods [[getColumn [] Integer]
	     [getMaxColumn [] Integer]
	     [setMaxColumn [Integer] Void]]
   :state state))

(def *default-page-width* 72)

(defn- -init 
  ([writer] (-init writer *default-page-width*))
  ([writer max-columns] [[] (ref {:max max-columns, :cur 0, :base writer})]))

(defn- get-field [this sym]
  (sym @(.state this)))

(defn- set-field [this sym new-val] 
  (alter (.state this) assoc sym new-val))

(defn- -getColumn [this]
  (get-field this :cur))

(defn- -getMaxColumn [this]
  (get-field this :max))

(defn- -setMaxColumn [this new-max]
  (dosync (set-field this :max new-max)))

(defn -write 
  ([this cbuf off len] 
     (.write (get-field this :base) cbuf off len))
  ([this x]
     (condp 
      =	    ;TODO put these back up when the parser understands condp 
      (class x)

      String 
      (let [s #^String x
	    nl (.lastIndexOf x (int \newline))]
	(dosync (if (neg? nl)
		  (set-field this :cur (+ (get-field this :cur) (count s)))
		  (set-field this :cur (- (count s) nl 1))))
	(.write (get-field this :base) s))

      Integer
      (let [c #^Character x]
	(dosync (if (= c (int \newline))
		  (set-field this :cur 0)
		  (set-field this :cur (inc (get-field this :cur)))))
	(.write (get-field this :base) c)))))

(defn- -flush [this]) ;; Currently a no-op

(defn- -close [this]) ;; Currently a no-op
