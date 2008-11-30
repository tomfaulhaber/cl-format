(ns format
  (:import [format FormatException InternalFormatException]))

(def teststr "The answer is ~7D.")
(def teststr1 "The answer is ~7,3,'*@D.")

;;; Forward references
(declare compile-format)
(declare execute-format)
(declare arg-navigator)
;;; End forward references

(defn cl-format 
  "An implementation of a (mostly) Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	navigator (struct arg-navigator args args 0) ]
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
    (if (not lis)
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
	[acc context]
      (recur new-context (conj acc result))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument navigators manage the argument list
;;; as the format statement moves through the list
;;; (possibly going forwards and backwards as it does so)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct arg-navigator :seq :rest :pos )

;; TODO Include position in error w/InternalFormatException
(defn next-arg [ navigator ]
  (let [ rst (:rest navigator) ]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (rest rst) (inc (:pos navigator)))]
      (throw (new Exception  "Not enough arguments for format definition")))))

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
;;; The table of directives we support, each with its params,
;;; properties, and the compilation function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We start with a couple of helpers
(defn process-directive-table-element [ [ char params flags & generator-fn ] ]
  [char, 
   {:directive char,
    :params `(array-map ~@params),
    :flags flags,
    :generator-fn (concat '(fn [ params ]) generator-fn) }])

(defmacro defdirectives 
  [ & directives ]
  `(def directive-table (hash-map ~@(mapcat process-directive-table-element directives))))

(defdirectives 
  (\A 
   [ :mincol [0 Integer] :colinc [1 Integer] :minpad [0 Integer] :padchar [\space Character] ] 
   #{ :at }
   (fn [ params arg-navigator offsets]
     (let [ [arg arg-navigator] (next-arg arg-navigator) 
	    base-output (print-str arg)
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
       arg-navigator)))
  (\% 
   [ :count [1 Integer] ] 
   #{ }
   (fn [ params arg-navigator offsets]
     (dotimes [i (:count params)]
       (prn)
       arg-navigator)))
)
 
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
     
  ;;TODO this isn't handling nil paramters right (should set them to the default)
  (merge ; create the result map
   (into (array-map)  ; start with the default values, make sure the order is right
	 (reverse (for [[name [default]] (:params def)] [name [default offset]])))
   (reduce #(apply assoc %1 %2) {} (filter #(nth % 1) (zipmap (keys (:params def)) params))) ; add the specified parameters, filtering out nils
   flags)) ; and finally add the flags

(defn compile-directive [s offset]
  (let [[raw-params [rest offset]] (extract-params s offset)
	[_ [rest offset flags]] (extract-flags rest offset)
	directive (nth rest 0)
	def (get directive-table directive)
	params (map-params def (map translate-param raw-params) flags offset)
	]
    [[ ((:generator-fn def) params) directive params] [ (subs rest 1) (inc offset)]]))
    
(defn compile-raw-string [s]
  [ (fn [_ a _] (print s) a) nil nil])

(defn translate-internal-exception [format-str e]
  (let [message (str (.getMessage e) \newline format-str \newline 
		     (apply str (replicate (.pos e) \space)) "^" \newline)]
    (throw (FormatException. message))))

(defn compile-format [ format-str ]
  (try
   (first (consume 
	   (fn [[s offset]]
	     (if (empty? s)
	       [nil s]
	       (let [tilde (.indexOf s (int \~))]
		 (cond
		  (neg? tilde) [(compile-raw-string s) ["" (+ offset (.length s))]]
		  (zero? tilde)  (compile-directive (subs s 1) (inc offset))
		  true 
		  [(compile-raw-string (subs s 0 tilde)) [(subs s tilde) tilde]]))))
	   [format-str 0]))
   (catch InternalFormatException e 
     (translate-internal-exception format-str e))
   (catch RuntimeException e ; Functions like map bury the thrown exception inside a
			     ; RuntimeException, so we have to dig it out.
     (let [cause (.getCause e)]
       (if (instance? InternalFormatException cause)
	 (translate-internal-exception format-str cause)
	 (throw))))))

(defn unzip-map [m]
  "Take a  map that has pairs in the value slots and produce a pair of maps, 
   the first having all the first elements of the pairs and the second all 
   the second elements of the pairs"
  [(into {} (for [[k [v1 v2]] m] [k v1]))
   (into {} (for [[k [v1 v2]] m] [k v2]))])

(defn execute-format [stream format args]
  (let [real-stream (cond 
		     (not stream) (new java.io.StringWriter)
		     (= stream true) *out*
		     true stream)]
	(binding [*out* real-stream]
	  (map-passing-context 
	   (fn [element context] 
	     (let [[params args] (realize-parameter-list (nth element 2) context)
		   [params offsets] (unzip-map params)]
	       [nil (apply (first element) [params args offsets])] ))
	   args
	   format)
	  (if (not stream) (.toString real-stream)))))


