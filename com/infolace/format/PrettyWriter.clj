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
   :constructors {[java.io.Writer Integer Object] [java.io.Writer Integer]}
   :methods [[startBlock [String String String] void]
	     [endBlock [] void]
	     [newline [clojure.lang.Keyword] void]
	     [indent [clojure.lang.Keyword Integer] void]
	     [getMiserWidth [] Object]
	     [setMiserWidth [Object] void]]
   :exposes-methods {write col-write}
   :state pwstate))

;; TODO: Support for tab directives

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros to simplify dealing with types and classes. These are
;;; really utilities, but I'm experimenting with them here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro #^{:private true} 
  getf 
  "Get the value of the field a named by the argument (which should be a keyword)."
  [sym]
  `(~sym @(.pwstate ~'this)))

(defmacro #^{:private true} 
  setf [sym new-val] 
  "Set the value of the field SYM to NEW-VAL"
  `(alter (.pwstate ~'this) assoc ~sym ~new-val))

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

(defstruct #^{:private true} logical-block
	   :parent :section :start-col :indent
	   :done-nl :intra-block-nl
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
(deftype buffer-blob :data :trailing-white-space)
(defmethod blob-length :buffer-blob [b]
  (+
   (count (:data b))
   (count (:trailing-white-space b))))

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
   (let [lb (struct logical-block nil nil (ref 0) (ref 0) (ref false) (ref false))]
     (ref {:logical-blocks lb 
	   :sections nil
	   :mode :writing
	   :buffer []
	   :buffer-block lb
	   :buffer-level 1
	   :miser-width miser-width
	   :trailing-white-space nil}))])

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
    (emit-nl this token)
    (if-let [tws (getf :trailing-white-space)]
      (.col-write this tws)))
  (dosync (setf :trailing-white-space nil)))

(defn- write-tokens [this tokens]
  (doseq [token tokens]
    (if-not (= (:type-tag token) :nl)
	    (if-let [tws (getf :trailing-white-space)]
	      (.col-write this tws)))
    (write-token this token)
    (setf :trailing-white-space (:trailing-white-space token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emit-nl? method defs for each type of new line. This makes
;;; the decision about whether to print this type of new line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- tokens-fit? [this tokens]
;  (prlabel tf? (.getColumn this) (buffer-length tokens))
  (< (+ (.getColumn this) (buffer-length tokens))
     (.getMaxColumn this)))

(defn- linear-nl? [this lb section]
;  (prlabel lnl? @(:done-nl lb) (tokens-fit? this section))
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

(defmethod emit-nl? :fill [newl this section subsection]
  (let [lb (:logical-block newl)]
    (or @(:intra-block-nl lb)
	(not (tokens-fit? this subsection))
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

(defn- update-nl-state [lb]
  (dosync
   (ref-set (:intra-block-nl lb) false)
   (ref-set (:done-nl lb) false)
   (loop [lb (:parent lb)]
     (if lb
       (do (ref-set (:done-nl lb) true)
	   (ref-set (:intra-block-nl lb) true)
	   (recur (:parent lb)))))))

(defn emit-nl [this nl]
  (.col-write this (int \newline))
  (dosync (setf :trailing-white-space nil))
  (let [lb (:logical-block nl)
	prefix (:per-line-prefix lb)] 
    (if prefix 
      (.col-write this prefix))
    (.col-write this (apply str (replicate (- @(:indent lb) (count prefix))
					   \space)))
    (update-nl-state lb)))

(defn- split-at-newline [tokens]
  (let [pre (take-while #(not (nl? %)) tokens)]
    [pre (drop (count pre) tokens)]))

;;; Methods for showing token strings for debugging

(defmulti tok :type-tag)
(defmethod tok :nl [token]
  (:type token))
(defmethod tok :buffer-blob [token]
  (str \" (:data token) (:trailing-white-space token) \"))
(defmethod tok :default [token]
  (:type-tag token))
(defn toks [toks] (map tok toks))

;;; write-token-string is called when the set of tokens in the buffer
;;; is longer than the available space on the line

(defn- write-token-string [this tokens]
  (let [[a b] (split-at-newline tokens)]
;;    (prlabel wts (toks a) (toks b))
    (if a (write-tokens this a))
    (if b
      (let [[section remainder] (get-section b)
	    newl (first b)]
;; 	(prlabel wts (toks section)) (prlabel wts (:type newl)) (prlabel wts (toks remainder)) 
	(let [do-nl (emit-nl? newl this section (get-sub-section b))
	      result (if do-nl 
		       (do
;; 			 (prlabel emit-nl (:type newl))
			 (emit-nl this newl)
			 (rest b))
		       b)
	      long-section (not (tokens-fit? this result))
	      result (if long-section
		       (let [rem2 (write-token-string this section)]
;;; 			     (prlabel recurse (toks rem2))
			 (if (= rem2 section)
			   (do ; If that didn't produce any output, it has no nls
					; so we'll force it
			     (write-tokens this section)
			     remainder)
			   (into [] (concat rem2 remainder))))
		       result)
;;	      ff (prlabel wts (toks result))
	      ] 
	  result)))))

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
	     (add-to-buffer this (make-buffer-blob (first lines) nil))
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


(defn write-white-space [this]
  (if-let [tws (getf :trailing-white-space)]
    (dosync
     (.col-write this tws)
     (setf :trailing-white-space nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writer overrides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -write 
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
	    (add-to-buffer this (make-buffer-blob (str (char c)) nil))))))))

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
   (let [lb (struct logical-block (getf :logical-blocks) nil (ref 0) (ref 0)
		    (ref false) (ref false)
		    prefix per-line-prefix suffix)]
     (setf :logical-blocks lb)
     (if (= (getf :mode) :writing)
       (do
	 (write-white-space this)
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
       (do
	 (write-white-space this)
	 (if-let [suffix (:suffix lb)]
	   (.col-write this suffix)))
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
       (do
	 (write-white-space this)
	 (ref-set (:indent lb) 
		  (+ offset (condp 
			     = relative-to
			     :block @(:start-col lb)
			     :current (.getColumn this)))))
       (add-to-buffer this (make-indent lb relative-to offset))))))

(defn- -getMiserWidth [this]
  (getf :miser-width))

(defn- -setMiserWidth [this new-miser-width]
  (dosync (setf :miser-width new-miser-width)))

