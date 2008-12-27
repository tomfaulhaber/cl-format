;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format
  (:import [com.infolace.format FormatException InternalFormatException]))

;;; Forward references
(declare compile-format)
(declare execute-format)
(declare init-navigator)
;;; End forward references

(defn cl-format 
  "An implementation of a (mostly) Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	navigator (init-navigator args) ]
    (execute-format stream compiled-format navigator))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for digesting formats in the various
;;; phases of their lives.
;;; These functions are actually pretty general.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn map-passing-context [func initial-context lis]
  (loop [context initial-context
	 lis lis
	 acc []]
    (if (empty? lis)
      [acc context]
    (let [this (first lis)
	  remainder (rest lis)
	  [result new-context] (apply func [this context])]
      (recur new-context remainder (conj acc result))))))

(defn consume [func initial-context]
  (loop [context initial-context
	 acc []]
    (let [[result new-context] (apply func [context])]
      (if (not result)
	[acc new-context]
      (recur new-context (conj acc result))))))

(defn consume-while [func initial-context]
  (loop [context initial-context
	 acc []]
    (let [[result continue new-context] (apply func [context])]
      (if (not continue)
	[acc context]
      (recur new-context (conj acc result))))))

(defn unzip-map [m]
  "Take a  map that has pairs in the value slots and produce a pair of maps, 
   the first having all the first elements of the pairs and the second all 
   the second elements of the pairs"
  [(into {} (for [[k [v1 v2]] m] [k v1]))
   (into {} (for [[k [v1 v2]] m] [k v2]))])

(defn tuple-map [m v1]
  "For all the values, v, in the map, replace them with [v v1]"
  (into {} (for [[k v] m] [k [v v1]])))

(defn rtrim [s c]
  "Trim all instances of c from the end of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s (dec (count s))) c))
      (loop [n (dec len)]
	(cond 
	 (neg? n) ""
	 (not (= (nth s n) c)) (subs s 0 (inc n))
	 true (recur (dec n))))
      s)))

(defn ltrim [s c]
  "Trim all instances of c from the beginning of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s 0) c))
      (loop [n 0]
	(if (or (= n len) (not (= (nth s n) c)))
	  (subs s n)
	  (recur (inc n))))
      s)))

(defn prefix-count [aseq val]
  "Return the number of times that val occurs at the start of sequence aseq"
  (loop [pos 0]
    (if (or (= pos (count aseq)) (not (= (nth aseq pos) val)))
      pos
      (recur (inc pos)))))

(defn prerr [& args]
  (binding [*out* *err*]
    (apply println args)))
       
(defmacro prlabel [prefix arg & more-args]
  "Print args to *err* in name = value format"
  (cons 'prerr (cons (list 'quote prefix) (mapcat #(list (list 'quote %) "=" %) 
						  (cons arg more-args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument navigators manage the argument list
;;; as the format statement moves through the list
;;; (possibly going forwards and backwards as it does so)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct arg-navigator :seq :rest :pos )

(defn init-navigator [s]
  "Create a new arg-navigator from the sequence with the position set to 0"
  (struct arg-navigator s s 0))

;; TODO Include position in error w/InternalFormatException
(defn next-arg [ navigator ]
  (let [ rst (:rest navigator) ]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (rest rst) (inc (:pos navigator)))]
      (throw (new Exception  "Not enough arguments for format definition")))))

(defn next-arg-or-nil [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (rest rst) (inc (:pos navigator)))]
      [nil navigator])))

;; Get an argument off the arg list and compile it if it's not already compiled
(defn get-format-arg [navigator]
  (let [[raw-format navigator] (next-arg navigator)
	compiled-format (if (instance? String raw-format) 
			       (compile-format raw-format)
			       raw-format)]
    [compiled-format navigator]))

(declare relative-reposition)

(defn absolute-reposition [navigator position]
  (if (>= position (:pos navigator))
    (relative-reposition navigator (- (:pos navigator) position))
    (struct arg-navigator (:seq navigator) (drop position (:seq navigator)) position)))

(defn relative-reposition [navigator position]
  (let [newpos (+ (:pos navigator) position)]
    (if (neg? position)
      (absolute-reposition navigator newpos)
      (struct arg-navigator (:seq navigator) (drop position (:rest navigator)) newpos))))

(defstruct compiled-directive :func :def :params :offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When looking at the parameter list, we may need to manipulate
;;; the argument list as well (for 'V' and '#' parameter types).
;;; We hide all of this behind a function, but clients need to
;;; manage changing arg navigator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: validate parameters when they come from arg list
(defn realize-parameter [[param [raw-val offset]] navigator]
  (let [[real-param new-navigator]
	(cond 
	 (contains? #{ :at :colon } param) ;pass flags through unchanged - this really isn't necessary
	 [raw-val navigator]

	 (= raw-val :parameter-from-args) 
	 (next-arg navigator)

	 (= raw-val :remaining-arg-count) 
	 [(count (:rest navigator)) navigator]

	 true 
	 [raw-val navigator])]
    [[param [real-param offset]] new-navigator]))
	 
(defn realize-parameter-list [parameter-map navigator]
  (let [[pairs new-navigator] 
	(map-passing-context realize-parameter navigator parameter-map)]
    [(into {} pairs) new-navigator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that support individual directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common handling code for ~A and ~S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn format-ascii [print-func params arg-navigator offsets]
  (let [ [arg arg-navigator] (next-arg arg-navigator) 
	 base-output (print-func arg)
	 base-width (.length base-output)
	 min-width (+ base-width (:minpad params))
	 width (if (>= min-width (:mincol params)) 
		 min-width
		 (+ min-width 
		    (* (+ (quot (- (:mincol params) min-width 1) 
				(:colinc params) )
			  1)
		       (:colinc params))))
	 chars (apply str (replicate (- width base-width) (:padchar params)))]
    (if (:at params)
      (print (str chars base-output))
      (print (str base-output chars)))
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the integer directives ~D, ~X, ~O, ~B and some
;;; of ~R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remainders [base val]
  "Return the list of remainders (essentially the 'digits') of val in the given base"
  (reverse 
   (first 
    (consume #(if (pos? %) 
		[(rem % base) (quot % base)] 
		[nil nil]) 
	     val))))

(defn base-str [base val]
  "Return val as a string in the given base"
  (apply str 
	 (map 
	  #(if (< % 10) (char (+ (int \0) %)) (char (+ (int \a) (- % 10)))) 
	  (remainders base val))))

(def java-base-formats {8 "%o", 10 "%d", 16 "%x"})

(defn opt-base-str [base val]
  "Return val as a string in the given base, using clojure.core/format if supported
for improved performance"
  (let [format-str (get java-base-formats base)]
    (if format-str
      (clojure.core/format format-str val)
      (base-str base val))))

(defn group-by [unit lis]
  (reverse
   (first
    (consume (fn [x] [(reverse (take unit x)) (drop unit x)]) (reverse lis)))))

(defn format-integer [base params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator) 
	neg (neg? arg)
	pos-arg (if neg (- arg) arg)
	raw-str (opt-base-str base pos-arg)
	group-str (if (:colon params)
		    (let [groups (map #(apply str %) (group-by (:commainterval params) raw-str))
			  commas (replicate (count groups) (:commachar params))]
		      (apply str (rest (interleave commas groups))))
		    raw-str)
	signed-str (cond
		    neg (str "-" group-str)
		    (:at params) (str "+" group-str)
		    true group-str)
	padded-str (if (< (.length signed-str) (:mincol params))
		     (str (apply str (replicate (- (:mincol params) (.length signed-str)) 
						(:padchar params)))
			  signed-str)
		     signed-str)]
    (print padded-str)
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for english formats (~R and ~:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn format-cardinal-english [params arg-navigator offsets]
  (throw (FormatException. "Cardinal english numbers with ~R not implemented yet")))

(defn format-ordinal-english [params arg-navigator offsets]
  (throw (FormatException. "Ordinal english numbers with ~:R not implemented yet")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for roman numeral formats (~@R and ~@:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn format-old-roman [params arg-navigator offsets]
  (throw (FormatException. "Old Roman numerals with ~@:R not implemented yet")))

(defn format-new-roman [params arg-navigator offsets]
  (throw (FormatException. "New Roman numerals with ~@R not implemented yet")))

;; Check to see if a result is an abort (~^) construct
;; TODO: move these funcs somewhere more appropriate
(defn abort-p [context]
  (let [token (first context)]
    (or (= :up-arrow token) (= :colon-up-arrow token))))

;; Handle the execution of "sub-clauses" in bracket constructions
(defn execute-sub-format [format args base-args]
  (frest
   (map-passing-context 
    (fn [element context]
      (if (abort-p context)
	[nil context] ; just keep passing it along
	(let [[params args] (realize-parameter-list (:params element) context)
	      [params offsets] (unzip-map params)
	      params (assoc params :base-args base-args)]
	  [nil (apply (:func element) [params args offsets])])))
    args
    format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for real number formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn float-parts-base
  "Produce string parts for the mantissa (normalized 1-9) and exponent"
  [f]
  (let [s (.toLowerCase (.toString f))
	exploc (.indexOf s (int \e))]
    (if (neg? exploc)
      (let [dotloc (.indexOf s (int \.))]
	(if (neg? dotloc)
	  [s (.toString (dec (count s)))]
	  [(str (subs s 0 dotloc) (subs s (inc dotloc))) (dec dotloc)]))
      [(str (subs s 0 1) (subs s 2 exploc)) (subs s (inc exploc))])))


(defn float-parts [f]
  "Take care of leading and trailing zeros in decomposed floats"
  (let [[m e] (float-parts-base f)
	m1 (rtrim m \0)
	m2 (ltrim m1 \0)
	delta (- (count m1) (count m2))]
    (if (empty? m2)
      ["0" 0]
      [m2 (- (Integer/valueOf e) delta)])))

(defn round-str [m e d w]
  (if (or d w)
    (let [len (count m)
	  round-pos (if d (+ e d 1))
	  round-pos (if (and w (< (inc e) (dec w)) 
			     (or (nil? round-pos) (< (dec w) round-pos)))
		      (dec w)
		      round-pos)
	  [m1 e1 round-pos len] (if (= round-pos 0) 
				  [(str "0" m) (inc e) 1 (inc len)]
				  [m e round-pos len])]
      (if round-pos
	(if (> len round-pos)
	  (let [round-char (nth m1 round-pos)
		result (subs m1 0 round-pos)]
	    (if (>= (int round-char) (int \5))
	      (let [result-val (Integer/valueOf result)
		    leading-zeros (subs result 0 (min (prefix-count result \0) (- round-pos 1)))
		    round-up-result (str leading-zeros
					 (String/valueOf (+ result-val 
							    (if (neg? result-val) -1 1))))
		    expanded (> (count round-up-result) (count result))]
		[round-up-result e1 expanded])
	      [result e1 false]))
	  [m e false])
	[m e false]))
    [m e false]))

(defn expand-fixed [m e d]
  (let [m1 (if (neg? e) (str (apply str (replicate (dec (- e)) \0)) m) m)
	len (count m1)
	target-len (if d (+ e d 1) (inc e))]
    (if (< len target-len) 
      (str m1 (apply str (replicate (- target-len len) \0))) 
      m1)))

(defn insert-decimal [m e]
  "Insert the decimal point at the right spot in the number to match an exponent"
  (if (neg? e)
    (str "." m)
    (let [loc (inc e)]
      (str (subs m 0 loc) "." (subs m loc)))))

(defn get-fixed [m e d]
  (insert-decimal (expand-fixed m e d) e))

(defn insert-scaled-decimal [m k]
  "Insert the decimal point at the right spot in the number to match an exponent"
  (if (neg? k)
    (str "." m)
    (str (subs m 0 k) "." (subs m k))))

;; the function to render ~F directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
(defn fixed-float [params navigator offsets]
  (let [w (:w params)
	d (:d params)
	[arg navigator] (next-arg navigator)
	[mantissa exp] (float-parts arg)
	scaled-exp (+ exp (:k params))
	add-sign (and (:at params) (not (neg? arg)))
	prepend-zero (< (Math/abs arg) 1.0)
	append-zero (and (not d) (<= (dec (count mantissa)) scaled-exp))
	[rounded-mantissa scaled-exp] (round-str mantissa scaled-exp 
						 d (if w (- w (if add-sign 1 0))))
	fixed-repr (get-fixed rounded-mantissa scaled-exp d)]
    (if w
      (let [len (count fixed-repr)
	    signed-len (if add-sign (inc len) len)
	    prepend-zero (and prepend-zero (not (= signed-len w)))
	    append-zero (and append-zero (not (= signed-len w)))
	    full-len (if (or prepend-zero append-zero)
		       (inc signed-len) 
		       signed-len)]
	(if (and (> full-len w) (:overflowchar params))
	  (print (apply str (replicate w (:overflowchar params))))
	  (print (str
		  (apply str (replicate (- w full-len) (:padchar params)))
		  (if add-sign "+") 
		  (if prepend-zero "0")
		  fixed-repr
		  (if append-zero "0")))))
      (print (str
	      (if add-sign "+") 
	      (if prepend-zero "0")
	      fixed-repr
	      (if append-zero "0"))))
    navigator))


;; the function to render ~E directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
;; TODO: define ~E representation for Infinity
(defn exponential-float [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (loop [[mantissa exp] (float-parts (Math/abs arg))]
      (let [w (:w params)
	    d (:d params)
	    e (:e params)
	    k (:k params)
	    expchar (or (:exponentchar params) \E)
	    add-sign (or (:at params) (neg? arg))
	    prepend-zero (<= k 0)
	    scaled-exp (- exp (dec k))
	    scaled-exp-str (str (Math/abs scaled-exp))
	    scaled-exp-str (str expchar (if (neg? scaled-exp) \- \+) 
				(if e (apply str 
					     (replicate 
					      (- e 
						 (count scaled-exp-str)) 
					      \0))) 
				scaled-exp-str)
	    exp-width (count scaled-exp-str)
	    base-mantissa-width (count mantissa)
	    scaled-mantissa (str (apply str (replicate (- k) \0))
				 mantissa
				 (if d 
				   (apply str 
					  (replicate 
					   (- d (dec base-mantissa-width)
					      (if (neg? k) (- k) 0)) \0))))
	    w-mantissa (if w (- w exp-width))
	    [rounded-mantissa _ incr-exp] (round-str 
					   scaled-mantissa 0
					   (cond
					    (= k 0) (dec d)
					    (pos? k) d
					    (neg? k) (dec d))
					   (if w-mantissa 
					     (- w-mantissa (if add-sign 1 0))))
	    full-mantissa (insert-scaled-decimal rounded-mantissa k)
	    append-zero (and (= k (count rounded-mantissa)) (nil? d))]
	(if (not incr-exp)
	  (if w
	    (let [len (+ (count full-mantissa) exp-width)
		  signed-len (if add-sign (inc len) len)
		  prepend-zero (and prepend-zero (not (= signed-len w)))
		  full-len (if prepend-zero (inc signed-len) signed-len)
		  append-zero (and append-zero (< full-len w))]
	      (if (and (or (> full-len w) (and e (> (- exp-width 2) e)))
		       (:overflowchar params))
		(print (apply str (replicate w (:overflowchar params))))
		(print (str
			(apply str 
			       (replicate 
				(- w full-len (if append-zero 1 0) )
				(:padchar params)))
			(if add-sign (if (neg? arg) \- \+)) 
			(if prepend-zero "0")
			full-mantissa
			(if append-zero "0")
			scaled-exp-str))))
	    (print (str
		    (if add-sign (if (neg? arg) \- \+)) 
		    (if prepend-zero "0")
		    full-mantissa
		    (if append-zero "0")
		    scaled-exp-str)))
	  (recur [rounded-mantissa (inc exp)]))))
    navigator))

;; the function to render ~G directives
;; This just figures out whether to pass the request off to ~F or ~E based 
;; on the algorithm in CLtL.
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
;; TODO: refactor so that float-parts isn't called twice
(defn general-float [params navigator offsets]
  (let [[arg _] (next-arg navigator)
	[mantissa exp] (float-parts (Math/abs arg))
	w (:w params)
	d (:d params)
	e (:e params)
	n (if (= arg 0.0) 0 (inc exp))
	ee (if e (+ e 2) 4)
	ww (if w (- w ee))
	d (if d d (max (count mantissa) (min n 7)))
	dd (- d n)]
    (if (<= 0 dd d)
      (let [navigator (fixed-float {:w ww, :d dd, :k 0, 
				    :overflowchar (:overflowchar params),
				    :padchar (:padchar params), :at (:at params)} 
				   navigator offsets)]
	(print (apply str (replicate ee \space)))
	navigator)
      (exponential-float params navigator offsets))))

;; the function to render ~$ directives
;; TODO: support rationals. Back off to ~D/~A is the appropriate cases
(defn dollar-float [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)
	[mantissa exp] (float-parts (Math/abs arg))
	d (:d params) ; digits after the decimal
	n (:n params) ; minimum digits before the decimal
	w (:w params) ; minimum field width
	add-sign (and (:at params) (not (neg? arg)))
	[rounded-mantissa scaled-exp _] (round-str mantissa exp d nil)
	fixed-repr (get-fixed rounded-mantissa scaled-exp d)
	full-repr (str (apply str (replicate (- n (.indexOf fixed-repr (int \.))) \0)) fixed-repr)
	full-len (+ (count full-repr) (if add-sign 1 0))]
    (print (str
	    (if (and (:colon params) add-sign) (if (neg? arg) \- \+))
	    (apply str (replicate (- w full-len) (:padchar params)))
	    (if (and (not (:colon params)) add-sign) (if (neg? arg) \- \+))
	    full-repr))
    navigator))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the '~[...~]' conditional construct in its
;;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~[...~] without any modifiers chooses one of the clauses based on the param or 
;; next argument
;; TODO check arg is positive int
(defn choice-conditional [params arg-navigator offsets]
  (let [arg (:selector params)
	[arg navigator] (if arg [arg arg-navigator] (next-arg arg-navigator))
	clauses (:clauses params)
	clause (if (or (neg? arg) (>= arg (count clauses)))
		 (first (:else params))
		 (nth clauses arg))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~:[...~] with the colon reads the next argument treating it as a truth value
(defn boolean-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
	clauses (:clauses params)
	clause (if arg
		 (frest clauses)
		 (first clauses))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~@[...~] with the at sign executes the conditional if the next arg is not
;; nil/false without consuming the arg
(defn check-arg-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
	clauses (:clauses params)
	clause (if arg (first clauses))]
    (if arg
      (if clause
	(execute-sub-format clause arg-navigator (:base-args params))
	arg-navigator)
      navigator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for the '~{...~}' iteration construct in its
;;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ~{...~} without any modifiers uses the next argument as an argument list that 
;; is consumed by all the iterations
(defn iterate-sublist [params navigator offsets]
  (let [max-count (:max-iterations params)
	param-clause (first (:clauses params))
	[clause navigator] (if (empty? param-clause) 
			     (get-format-arg navigator)
			     [param-clause navigator]) 
	[arg-list navigator] (next-arg navigator)
	args (init-navigator arg-list)]
    (loop [count 0
	   args args
	   last-pos -1]
      (if (and (not max-count) (= (:pos args) last-pos) (> count 1))
	(throw (FormatException. "%{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest args))
		   (or (not (:colon-right params)) (> count 0)))
	      (and max-count (>= count max-count)))
	navigator
	(recur (inc count) 
	       (execute-sub-format clause args (:base-args params))
	       (:pos args))))))

;; ~:{...~} with the colon treats the next argument as a list of sublists. Each of the
;; sublists is used as the arglist for a single iteration.
(defn iterate-list-of-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
	param-clause (first (:clauses params))
	[clause navigator] (if (empty? param-clause) 
			     (get-format-arg navigator)
			     [param-clause navigator]) 
	[arg-list navigator] (next-arg navigator)]
    (loop [count 0
	   arg-list arg-list]
      (if (or (and (empty? arg-list)
		   (or (not (:colon-right params)) (> count 0)))
	      (and max-count (>= count max-count)))
	navigator
	(let [iter-result (execute-sub-format 
			   clause 
			   (init-navigator (first arg-list))
			   (init-navigator (rest arg-list)))]
	  (if (= :colon-up-arrow (first iter-result))
	    navigator
	    (recur (inc count) (rest arg-list))))))))

;; ~@{...~} with the at sign uses the main argument list as the arguments to the iterations
;; is consumed by all the iterations
(defn iterate-main-list [params navigator offsets]
  (let [max-count (:max-iterations params)
	param-clause (first (:clauses params))
	[clause navigator] (if (empty? param-clause) 
			     (get-format-arg navigator)
			     [param-clause navigator])]
    (loop [count 0
	   navigator navigator
	   last-pos -1]
      (if (and (not max-count) (= (:pos navigator) last-pos) (> count 1))
	(throw (FormatException. "%@{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest navigator))
		   (or (not (:colon-right params)) (> count 0)))
	      (and max-count (>= count max-count)))
	navigator
	(recur 
	 (inc count) 
	 (execute-sub-format clause navigator (:base-args params)) 
	 (:pos navigator))))))

;; ~@:{...~} with both colon and at sign uses the main argument list as a set of sublists, one
;; of which is consumed with each iteration
(defn iterate-main-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
	param-clause (first (:clauses params))
	[clause navigator] (if (empty? param-clause) 
			     (get-format-arg navigator)
			     [param-clause navigator]) 
	]
    (loop [count 0
	   navigator navigator]
      (if (or (and (empty? (:rest navigator))
		   (or (not (:colon-right params)) (> count 0)))
	      (and max-count (>= count max-count)))
	navigator
	(let [[sublist navigator] (next-arg-or-nil navigator)]
	  (execute-sub-format clause (init-navigator sublist) navigator)
	  (recur (inc count) navigator))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The table of directives we support, each with its params,
;;; properties, and the compilation function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We start with a couple of helpers
(defn process-directive-table-element [ [ char params flags bracket-info & generator-fn ] ]
  [char, 
   {:directive char,
    :params `(array-map ~@params),
    :flags flags,
    :bracket-info bracket-info,
    :generator-fn (concat '(fn [ params offset]) generator-fn) }])

(defmacro defdirectives 
  [ & directives ]
  `(def directive-table (hash-map ~@(mapcat process-directive-table-element directives))))

(defdirectives 
  (\A 
   [ :mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character] ] 
   #{ :at :colon :both} {}
   #(format-ascii print-str %1 %2 %3))

  (\S 
   [ :mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character] ] 
   #{ :at :colon :both} {}
   #(format-ascii pr-str %1 %2 %3))

  (\D
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 10 %1 %2 %3))

  (\B
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 2 %1 %2 %3))

  (\O
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 8 %1 %2 %3))

  (\X
   [ :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   #(format-integer 16 %1 %2 %3))

  (\R
   [:base [nil Integer] :mincol [0 Integer] :padchar [\space Character] :commachar [\, Character] 
    :commainterval [ 3 Integer]]
   #{ :at :colon :both } {}
   (do
     (cond				; ~R is overloaded with bizareness
      (first (:base params))     #(format-integer (:base %1) %1 %2 %3)
      (and (:at params) (:colon params))   #(format-old-roman %1 %2 %3)
      (:at params)               #(format-new-roman %1 %2 %3)
      (:colon params)            #(format-ordinal-english %1 %2 %3)
      true                       #(format-cardinal-english %1 %2 %3))))

  (\P
   [ ]
   #{ :at :colon :both } {}
   (fn [params navigator offsets]
     (let [navigator (if (:colon params) (relative-reposition navigator -1) navigator)
	   strs (if (:at params) ["y" "ies"] ["" "s"])
	   [arg navigator] (next-arg navigator)]
       (print (if (= arg 1) (first strs) (frest strs)))
       navigator)))

  (\F
   [ :w [nil Integer] :d [nil Integer] :k [0 Integer] :overflowchar [nil Character] 
    :padchar [\space Character] ]
   #{ :at } {}
   fixed-float)

  (\E
   [ :w [nil Integer] :d [nil Integer] :e [nil Integer] :k [1 Integer] 
    :overflowchar [nil Character] :padchar [\space Character] 
    :exponentchar [nil Character] ]
   #{ :at } {}
   exponential-float)

  (\G
   [ :w [nil Integer] :d [nil Integer] :e [nil Integer] :k [1 Integer] 
    :overflowchar [nil Character] :padchar [\space Character] 
    :exponentchar [nil Character] ]
   #{ :at } {}
   general-float)

  (\$
   [ :d [2 Integer] :n [1 Integer] :w [0 Integer] :padchar [\space Character]]
   #{ :at :colon :both} {}
   dollar-float)

  (\% 
   [ :count [1 Integer] ] 
   #{ } {}
   (fn [params arg-navigator offsets]
     (dotimes [i (:count params)]
       (prn))
     arg-navigator))

  (\~ 
   [ :n [1 Integer] ] 
   #{ } {}
   (fn [params arg-navigator offsets]
     (let [n (:n params)]
       (print (apply str (replicate n \~)))
       arg-navigator)))

  (\* 
   [ :n [1 Integer] ] 
   #{ :colon :at } {}
   (fn [params navigator offsets]
     (let [n (:n params)]
       (if (:at params)
	 (absolute-reposition navigator n)
	 (relative-reposition navigator (if (:colon params) (- n) n)))
       )))

  (\? 
   [ ] 
   #{ :at } {}
   (if (:at params)
     (fn [params navigator offsets] ; args from main arg list
       (let [[subformat navigator] (get-format-arg navigator)]
	 (execute-sub-format subformat navigator  (:base-args params))))
     (fn [params navigator offsets] ; args from sub-list
       (let [[subformat navigator] (get-format-arg navigator)
	     [subargs navigator] (next-arg navigator)
	     sub-navigator (init-navigator subargs)]
	 (execute-sub-format subformat sub-navigator (:base-args params))
	 navigator))))
       

  (\[
   [ :selector [nil Integer] ]
   #{ :colon :at } { :right \], :allows-separator true, :else :last }
   (cond
    (:colon params)
    boolean-conditional

    (:at params)
    check-arg-conditional

    true
    choice-conditional))

   (\; [] #{ :colon } { :separator true } nil) 
   
   (\] [] #{} {} nil) 

  (\{
   [ :max-iterations [nil Integer] ]
   #{ :colon :at :both} { :right \}, :allows-separator true, :else :last }
   (cond
    (and (:at params) (:colon params))
    iterate-main-sublists

    (:colon params)
    iterate-list-of-sublists

    (:at params)
    iterate-main-list

    true
    iterate-sublist))

   
   (\} [] #{:colon} {} nil) 

   ;; TODO: detect errors in cases where colon not allowed
   (\^ [:arg1 [nil Integer] :arg2 [nil Integer] :arg3 [nil Integer]] 
    #{:colon} {} 
    (fn [params navigator offsets]
     (let [arg1 (:arg1 params)
	   arg2 (:arg2 params)
	   arg3 (:arg3 params)
	   exit (if (:colon params) :colon-up-arrow :up-arrow)]
       (cond
	(and arg1 arg2 arg3)
	(if (<= arg1 arg2 arg3) [exit navigator] navigator)

	(and arg1 arg2)
	(if (= arg1 arg2) [exit navigator] navigator)

	arg1
	(if (= arg1 0) [exit navigator] navigator)

	true  ; TODO: handle looking up the arglist stack for info
	(if (if (:colon params) 
	      (nil? (:rest (:base-args params)))
	      (nil? (:rest navigator)))
	  [exit navigator] navigator))))) 
)

(defn my-status [] 
  (println "There are" (count directive-table) "out of 33 directives implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to manage the parameters and flags accociated with each
;;; directive in the format string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def param-pattern #"^([vV]|#|('.)|([+-]?\d+)|(?=,))")
(def special-params #{ :parameter-from-args :remaining-arg-count })

(defn extract-param [[s offset saw-comma]]
  (let [m (re-matcher param-pattern s)
	param (re-find m)]
    (if param
      (let [token-str (first (re-groups m))
	    remainder (subs s (.end m))
	    new-offset (+ offset (.end m))]
	(if (not (= \, (nth remainder 0)))
	  [ [token-str offset] [remainder new-offset false]]
	  [ [token-str offset] [(subs remainder 1) (inc new-offset) true]]))
      (if saw-comma 
	(throw (InternalFormatException. "Badly formed parameters in format directive" offset))
	[ nil [s offset]]))))


(defn extract-params [s offset] 
  (consume extract-param [s offset false]))

(defn translate-param [[p offset]]
  "Translate the string representation of a param to the internalized
  representation"
  [(cond 
    (= (.length p) 0) nil
    (and (= (.length p) 1) (contains? #{\v \V} (nth p 0))) :parameter-from-args
    (and (= (.length p) 1) (= \# (nth p 0))) :remaining-arg-count
    (and (= (.length p) 2) (= \' (nth p 0))) (nth p 1)
    true (new Integer p))
   offset])
 
(def flag-defs { \: :colon, \@ :at })

(defn extract-flags [s offset]
  (consume
   (fn [[s offset flags]]
    (if (empty? s)
      [nil [s offset flags]]
      (let [flag (get flag-defs (first s))]
	(if flag
	  (if (contains? flags flag)
	    (throw 
	     (InternalFormatException. 
	      (str "Flag \"" (first s) "\" appears more than once in a directive")
	      offset))
	    [true [(subs s 1) (inc offset) (assoc flags flag [true offset])]])
	  [nil [s offset flags]]))))
   [s offset {}]))

(defn check-flags [def flags]
  (let [allowed (:flags def)]
    (if (and (not (:at allowed)) (:at flags))
      (throw (InternalFormatException.
		  (str "\"@\" is an illegal flag for format directive \"" (:directive def) "\"")
		  (nth (:at flags) 1))))
    (if (and (not (:colon allowed)) (:colon flags))
      (throw (InternalFormatException.
		  (str "\":\" is an illegal flag for format directive \"" (:directive def) "\"")
		  (nth (:colon flags) 1))))
    (if (and (not (:both allowed)) (:at flags) (:colon flags))
      (throw (InternalFormatException.
		  (str "Cannot combine \"@\" and \":\" flags for format directive \"" 
		       (:directive def) "\"")
		  (min (nth (:colon flags) 1) (nth (:at flags) 1)))))))

(defn map-params [def params flags offset]
  "Takes a directive definition and the list of actual parameters and
   a map of flags and returns a map of the parameters and flags with defaults
   filled in. We check to make sure that there are the right types and number
   of parameters as well."
  (check-flags def flags)
  (if (> (count params) (count (:params def)))
    (throw (InternalFormatException.
	    (str "Too many parameters for directive \"" (:directive def) 
		 "\": specified " (count params) " but received " 
		 (count (:params def)))
	    (frest params))))

  (doall
   (map #(let [val (first %1)]
	   (if (not (or (nil? val) (contains? special-params val) 
			(instance? (frest (frest %2)) val)))
	     (throw (InternalFormatException.
		     (str "Parameter " (name (first %2))
			  " has bad type in directive \"" (:directive def) "\": "
			  (class val))
		     (frest %1)))) )
	params (:params def)))
     
  (merge ; create the result map
   (into (array-map)  ; start with the default values, make sure the order is right
	 (reverse (for [[name [default]] (:params def)] [name [default offset]])))
   (reduce #(apply assoc %1 %2) {} (filter #(first (nth % 1)) (zipmap (keys (:params def)) params))) ; add the specified parameters, filtering out nils
   flags)) ; and finally add the flags

(defn compile-directive [s offset]
  (let [[raw-params [rest offset]] (extract-params s offset)
	[_ [rest offset flags]] (extract-flags rest offset)
	directive (first rest)
	def (get directive-table (Character/toUpperCase directive))
	params (if def (map-params def (map translate-param raw-params) flags offset))
	]
    (if (not directive)
      (throw (InternalFormatException. "Format string ended in the middle of a directive" offset)))
    (if (not def)
      (throw (InternalFormatException. (str "Directive \"" directive "\" is undefined") offset)))
    [(struct compiled-directive ((:generator-fn def) params offset) def params offset)
     [ (subs rest 1) (inc offset)]]))
    
(defn compile-raw-string [s offset]
  (struct compiled-directive (fn [_ a _] (print s) a) nil nil offset))

(defn right-bracket [this] (:right (:bracket-info (:def this))))
(defn separator? [this] (:separator (:bracket-info (:def this))))
(defn else-separator? [this] 
  (and (:separator (:bracket-info (:def this)))
       (:colon (:params this))))
  

(declare collect-clauses)

(defn process-bracket [this remainder]
  (let [[subex remainder] (collect-clauses (:bracket-info (:def this))
					   (:offset this) remainder)]
    [(struct compiled-directive 
	     (:func this) (:def this) 
	     (merge (:params this) (tuple-map subex (:offset this)))
	     (:offset this))
     remainder]))

(defn process-clause [bracket-info offset remainder]
  (consume 
   (fn [remainder]
     (if (nil? remainder)
       (throw (InternalFormatException. "No closing bracket found." offset))
       (let [this (first remainder)
	     remainder (rest remainder)]
	 (cond
	  (right-bracket this)
	  (process-bracket this remainder)

	  (= (:right bracket-info) (:directive (:def this)))
	  [ nil [:right-bracket (:colon (:params this)) remainder]]

	  (else-separator? this)
	  [nil [:else nil remainder]]

	  (separator? this)
	  [nil [:separator nil remainder]]

	  true
	  [ this remainder]))))
   remainder))

(defn collect-clauses [bracket-info offset remainder]
  (frest
   (consume
    (fn [[clause-map saw-else remainder]]
      (let [[clause [type colon-right remainder]] 
	    (process-clause bracket-info offset remainder)]
	(cond
	 (= type :right-bracket)
	 [nil [(merge-with concat clause-map 
			   {(if saw-else :else :clauses) [clause] 
			    :colon-right colon-right})
	       remainder]]

	 (= type :else)
	 (cond
	  saw-else
	  (throw (InternalFormatException.
		  "Two else clauses (\"~:;\") inside bracket construction." offset))
	 
	  (not (:else bracket-info))
	  (throw (InternalFormatException.
		  "An else clause (\"~:;\") is in a bracket type that doesn't support it." 
		  offset))

	  (and (= :first (:else bracket-info)) (:clauses clause-map))
	  (throw (InternalFormatException.
		  "The else clause (\"~:;\") is only allowed in the first position for this directive." 
		  offset))
	 
	  true	  ; the else clause is next, this was a regular clause
	  ; TODO support ~:; first for justification
	  [true [(merge-with concat clause-map { :clauses [clause] })
		true remainder]])

	 (= type :separator)
	 (cond
	  saw-else
	  (throw (InternalFormatException.
		  "A plain clause (with \"~;\") follows an else clause (\"~:;\") inside bracket construction." offset))
	 
	  (not (:allows-separator bracket-info))
	  (throw (InternalFormatException.
		  "A separator (\"~;\") is in a bracket type that doesn't support it." 
		  offset))
	 
	  true
	  [true [(merge-with concat clause-map { :clauses [clause] })
		false remainder]]))))
    [{ :clauses [] } false remainder])))

(defn process-nesting [format]
  "Take a linearly compiled format and process the bracket directives to give it 
   the appropriate tree structure"
  (first
   (consume 
    (fn [remainder]
      (let [this (first remainder)
	    remainder (rest remainder)
	    bracket (:bracket-info (:def this))]
	(if (:right bracket)
	  (process-bracket this remainder)
	  [this remainder])))
    format)))

	    
      
(defn translate-internal-exception [format-str e]
  (let [message (str (.getMessage e) \newline format-str \newline 
		     (apply str (replicate (.pos e) \space)) "^" \newline)]
    (throw (FormatException. message))))

(defn compile-format [ format-str ]
  (try
   (process-nesting
    (first 
     (consume 
      (fn [[s offset]]
	(if (empty? s)
	  [nil s]
	  (let [tilde (.indexOf s (int \~))]
	    (cond
	     (neg? tilde) [(compile-raw-string s offset) ["" (+ offset (.length s))]]
	     (zero? tilde)  (compile-directive (subs s 1) (inc offset))
	     true 
	     [(compile-raw-string (subs s 0 tilde) offset) [(subs s tilde) tilde]]))))
      [format-str 0])))
   (catch InternalFormatException e 
     (translate-internal-exception format-str e))
   (catch RuntimeException e ; Functions like map bury the thrown exception inside a
			     ; RuntimeException, so we have to dig it out.
     (let [cause (.getCause e)]
       (if (instance? InternalFormatException cause)
	 (translate-internal-exception format-str cause)
	 (throw (RuntimeException. (.getMessage e) e)))))))


(defn execute-format [stream format args]
  (let [real-stream (cond 
		     (not stream) (new java.io.StringWriter)
		     (= stream true) *out*
		     true stream)]
	(binding [*out* real-stream]
	  (map-passing-context 
	   (fn [element context]
	     (if (abort-p context)
	       [nil context]
	       (let [[params args] (realize-parameter-list 
				    (:params element) context)
		     [params offsets] (unzip-map params)
		     params (assoc params :base-args args)]
		 [nil (apply (:func element) [params args offsets])])))
	   args
	   format)
	  (if (not stream) (.toString real-stream)))))


