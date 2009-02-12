;   Copyright (c) Tom Faulhaber, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.PrettyWriter
  (:use com.infolace.format.utilities)
  (:gen-class
   :extends com.infolace.format.ColumnWriter
   :init init
   :constructors {[java.io.Writer Integer Object] [java.io.Writer]}
   :methods [[startBlock [String String String] void]
	     [endBlock [] void]
	     [newline [clojure.lang.Keyword] void]
	     [indent [clojure.lang.Keyword Integer] void]
	     [getMiserWidth [] Object]
	     [setMiserWidth [Object] void]]
   :exposes-methods {write col-write}
   :state state))

;; TODO: support all newline types
;; TODO: Support for tab directives
;; TODO: Trim whitespace before newlines

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

(defmacro deftype [type-name & fields]
  (let [name-str (name type-name)]
    `(do
       (defstruct ~type-name :type-tag ~@fields)
       (defn- ~(symbol (str "make-" name-str)) 
	 [& vals#] (apply struct ~type-name ~(keyword name-str) vals#))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The data structures used by PrettyWriter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct #^{:private true} logical-block :parent :section :start-col :indent :done-nl
	   :prefix :per-line-prefix :suffix)

(defn ancestor? [parent child]
  (loop [child (:parent child)]
    (cond 
     (nil? child) false
     (= parent child) true
     :else (recur (:parent child)))))

(defstruct #^{:private true} section :parent)

(defmulti blob-length :type-tag)
(defmethod blob-length :default [_] 0)

(defn buffer-length [l] (reduce + (map blob-length l)))

; A blob of characters (aka a string)
(deftype buffer-blob :data)
(defmethod blob-length :buffer-blob [b] (count (:data b)))

; A newline
(deftype nl :type :logical-block)

(deftype start-block :logical-block)
(defmethod blob-length :start-block [b] (count (:prefix (:logical-block b))))

(deftype end-block :logical-block)
(defmethod blob-length :end-block [b] (count (:suffix (:logical-block b))))

(deftype indent :logical-block :relative-to :offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the PrettyWriter instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -init 
  [writer max-columns miser-width]
  [[writer max-columns] 
   (let [lb (struct logical-block nil nil (ref 0) (ref 0) (ref false))]
     (ref {:logical-blocks lb 
	   :sections nil
	   :mode :writing
	   :buffer []
	   :buffer-block lb
	   :buffer-level 1
	   :miser-width miser-width
	   :max max-columns, :cur 0, :base writer}))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to write tokens in the output buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare emit-nl)

(defmulti write-token #(:type-tag %2))
(defmethod write-token :start-block [this token]
  (let [lb (:logical-block token)]
    (dosync
     (if-let [prefix (:prefix lb)] 
       (.col-write this prefix))
     (let [col (.getColumn this)]
       (ref-set (:start-col lb) col)
       (ref-set (:indent lb) col)))))

(defmethod write-token :end-block [this token]
  (if-let [suffix (:suffix (:logical-block token))] 
    (.col-write this suffix)))

(defmethod write-token :indent [this token]
  (let [lb (:logical-block token)]
    (ref-set (:indent lb) 
	     (+ (:offset token)
		(condp 
		 = (:relative-to token)
		 :block @(:start-col lb)
		 :current (.getColumn this))))))

(defmethod write-token :buffer-blob [this token]
  (.col-write this (:data token)))

(defmethod write-token :nl [this token]
  (if (and (not (= (:type token) :fill))
	   @(:done-nl (:logical-block token)))
    (emit-nl this token)))

(defn- write-tokens [this tokens]
  (doseq [token tokens]
    (write-token this token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emit-nl? method defs for each type of new line. This makes
;;; the decision about whether to print this type of new line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- tokens-fit? [this tokens]
  (<= (+ (.getColumn this) (buffer-length tokens))
      (.getMaxColumn this)))

(defn- linear-nl? [this lb section]
  (or @(:done-nl lb)
      (not (tokens-fit? this section))))

(defn- miser-nl? [this lb section]
  (let [miser-width (.getMiserWidth this)]
    (and miser-width
	 (>= @(:start-col lb) (- (.getMaxColumn this) miser-width))
	 (linear-nl? this lb section))))

(defmulti emit-nl? (fn [t _ _ _] (:type t)))

(defmethod emit-nl? :linear [newl this section _]
  (let [lb (:logical-block newl)]
    (linear-nl? this lb section)))

(defmethod emit-nl? :miser [newl this section _]
  (let [lb (:logical-block newl)]
    (miser-nl? this lb section)))

;;; TODO: support clause (b) of the definition -> break if previous section broke
(defmethod emit-nl? :fill [newl this section subsection]
  (let [lb (:logical-block newl)]
    (or (not (tokens-fit? this subsection))
	(miser-nl? this lb section))))

(defmethod emit-nl? :mandatory [_ _ _ _]
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- get-section [buffer]
  (let [nl (first buffer) 
	lb (:logical-block nl)
	section (take-while #(not (and (nl? %) (ancestor? (:logical-block %) lb)))
			    (rest buffer))]
    [section (drop (inc (count section)) buffer)])) 

(defn- get-sub-section [buffer]
  (let [nl (first buffer) 
	lb (:logical-block nl)
	section (take-while #(let [nl-lb (:logical-block %)]
			       (not (and (nl? %) (or (= nl-lb lb) (ancestor? nl-lb lb)))))
			    (rest buffer))]
    section)) 

(defn emit-nl [this nl]
  (.col-write this (int \newline))
  (let [lb (:logical-block nl)
	prefix (:per-line-prefix lb)] 
    (if prefix 
      (.col-write this prefix))
    (.col-write this (apply str (replicate (- @(:indent lb) (count prefix))
					   \space)))
    (dosync
     (ref-set (:done-nl lb) true))))

(defn- split-at-newline [tokens]
  (let [pre (take-while #(not (nl? %)) tokens)]
    [pre (drop (count pre) tokens)]))

(defn- write-token-string [this tokens]
  (let [[a b] (split-at-newline tokens)]
;;     (prlabel wts a b)
    (if a (write-tokens this a))
    (if b
      (let [[section remainder] (get-section b)
	    newl (first b)]
;; 	(prlabel wts section) (prlabel wts newl) (prlabel wts remainder) 
	(let [result (if (emit-nl? newl this section (get-sub-section b))
		       (do
;; 			 (prlabel emit-nl newl)
			 (emit-nl this newl)
			 (if (not (tokens-fit? this section))
			   (let [rem2 (write-token-string this section)]
;; 			     (prlabel wts rem2)
			     (if (= rem2 section)
			       (do ; If that didn't produce any output, it has no nls
					; so we'll force it
				 (write-tokens this section)
				 remainder)
			       (into rem2 remainder)))
			   false))
		       false)] 
	  (if-not (false? result)
		  result
		  (if remainder
		    (do 
;; 		      (prerr "wts returning remainder")
		      (write-tokens this section)
		      remainder)
		    section)))))))

(defn- write-line [this]
;;   (prerr "@wl")
  (dosync
   (loop [buffer (getf :buffer)]
;;      (prlabel wl1 buffer)
     (setf :buffer (into [] buffer))
     (if (not (tokens-fit? this buffer))
       (let [new-buffer (write-token-string this buffer)]
;; 	 (prlabel wl new-buffer)
	 (if-not (identical? buffer new-buffer)
		 (recur new-buffer)))))))

;;; Add a buffer token to the buffer and see if it's time to start
;;; writing
(defn- add-to-buffer [this token]
;  (prlabel a2b token)
  (dosync
   (setf :buffer (conj (getf :buffer) token))
   (if (not (tokens-fit? this (getf :buffer)))
     (write-line this))))

;;; Write all the tokens that have been buffered
(defn- write-buffered-output [this]
  (write-line this)
  (if-let [buf (getf :buffer)]
    (do
      (write-tokens this buf)
      (setf :buffer []))))

;;; If there are newlines in the string, print the lines up until the last newline, 
;;; making the appropriate adjustments. Return the remainder of the string
(defn- write-initial-lines 
  [this s] 
  (let [lines (.split s "\n" -1)]
    (if (= (count lines) 1)
      s
      (dosync 
       (let [prefix (:per-line-prefix (first (getf :logical-blocks)))] 
	 (if (= :buffering (getf :mode))
	   (do
	     (add-to-buffer this (make-buffer-blob (first lines)))
	     (write-buffered-output this))
	   (.col-write this (first lines)))
	 (.col-write this (int \newline))
	 (doseq [l (rest (butlast lines))]
	   (.col-write this l)
	   (.col-write this (int \newline))
	   (if prefix
	     (.col-write this prefix)))
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
      (let [s (write-initial-lines this x)
	    mode (getf :mode)]
	(if (= mode :writing)
	  (.col-write this s)
	  (add-to-buffer this (make-buffer-blob s))))

      Integer
      (let [c #^Character x]
	(if (= (getf :mode) :writing)
	  (.col-write this x)
	  (if (= c (int \newline))
	    (write-initial-lines this "\n")
	    (add-to-buffer this (make-buffer-blob (str (char c))))))))))

(defn- -flush [this]
  (if (= (getf :mode) :buffering)
    (dosync 
     (write-tokens this (getf :buffer))
     (setf :buffer []))))

(defn- -close [this]
  (-flush this))			;TODO: close underlying stream?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for PrettyWriter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -startBlock [this prefix per-line-prefix suffix]
  (dosync 
   (let [lb (struct logical-block (getf :logical-blocks) nil (ref 0) (ref 0) (ref false) 
		    prefix per-line-prefix suffix)]
     (setf :logical-blocks lb)
     (if (= (getf :mode) :writing)
       (do
	 (if prefix 
	   (.col-write this prefix))
	 (let [col (.getColumn this)]
	   (ref-set (:start-col lb) col)
	   (ref-set (:indent lb) col)))
       (add-to-buffer this (make-start-block lb))))))

(defn- -endBlock [this]
  (dosync
   (let [lb (getf :logical-blocks)]
     (if (= (getf :mode) :writing)
       (if-let [suffix (:suffix lb)]
	 (.col-write this suffix))
       (add-to-buffer this (make-end-block lb)))
     (setf :logical-blocks (:parent lb)))))

(defn- -newline [this type]
  (dosync 
   (setf :mode :buffering)
   (add-to-buffer this (make-nl type (getf :logical-blocks)))))

(defn- -indent [this relative-to offset]
  (dosync 
   (let [lb (getf :logical-blocks)]
     (if (= (getf :mode) :writing)
       (ref-set (:indent lb) 
		(+ offset (condp 
			   = relative-to
			   :block @(:start-col lb)
			   :current (.getColumn this))))
       (add-to-buffer this (make-indent lb relative-to offset))))))

(defn- -getMiserWidth [this]
  (getf :miser-width))

(defn- -setMiserWidth [this new-miser-width]
  (dosync (setf :miser-width new-miser-width)))

