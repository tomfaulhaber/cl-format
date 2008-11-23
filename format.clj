;; TODO: think through exception tyeps
;; TODO: thread format string position through to all exceptions and add 
;; positional errors

(ns format)


(def teststr "The answer is ~7D.")
(def teststr1 "The answer is ~7,3,'*@D.")

;;; Forward references
(def compile-format)
(def execute-format)
;;; End forward references

(defn cl-format 
  "An implementation of a (mostly) Common Lisp compatible format function"
  [stream format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
	arg-navigator { :seq args :rest args :pos 0 } ]
    (execute-format stream compiled-format arg-navigator))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Argument navigators manage the argument list
;;; as the format statement moves through the list
;;; (possibly going forwards and backwards as it does so)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct arg-navigator :seq :rest :pos )

(defn next-arg [ navigator ]
  (let [ rst (:rest navigator) ]
    (if rst
      [(first rst) (struct arg-navigator (:seq navigator ) (rest rst) (inc (:pos navigator)))]
      (throw (new Exception  "Not enough arguments for format definition")))))


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
   (fn [ arg-navigator ]
     (let [ [arg arg-navigator] (next-arg arg-navigator) ]
       (pr arg)				; TODO: support padding and columns
       arg-navigator)))
)
 
(def 
 #^{ :doc "The pattern that matches a format directive (~...X) in a control string" }
 directive-pattern
 #"~((([vV]|#|(?:'.)|([+-]?\d+)),)*([vV]|#|(?:'.)|([+-]?\d+)))?([@:])*[A-Za-z]")

(def param-pattern #"^([vV]|#|('.)|([+-]?\d+))")

;; TODO bug: handle skipped parameters
(defn extract-params [s] 
  (loop [rest s
	 m (re-matcher param-pattern rest)
	 acc []
	 ]
    (let [param (re-find m)]
      (if (and (not param) (not (first acc)))
	[ acc rest ]
	(if (not param)
	  (throw (new Exception (str "Badly formed parameters in format directive \"~" s "\"")))  
	  (let [token-str (first (re-groups m))
		rest (subs rest (.end m))
		acc (conj acc token-str)]
	    (if (not (= \, (nth rest 0)))
	      [ acc rest ]
	      (let [rest (subs rest 1)]
		(recur rest (re-matcher param-pattern rest) acc)))))))))

(defn translate-param [p]
  "Translate the string representation of a param to the internalized
  representation"
  (cond 
   (and (= (.length p) 1) (contains? #{\v \V} (nth p 0))) :parameter-from-args
   (and (= (.length p) 1) (= \# (nth p 0))) :remaining-arg-count
   (and (= (.length p) 2) (= \' (nth p 0))) (nth p 1)
   true (new Integer p)))
 
(def flag-defs { \: :colon, \@ :at })

(defn extract-flags [s] 
  (loop [rest s
	 flags {}]
    (if (= 0 (.length rest))
      [flags rest]
      (let [flag (get flag-defs (first rest))]
	(if (not flag)
	  [flags rest]
	  (if (contains? flags flag)
	    (throw (new Exception (str "Flag \"" flag "\" appears more than once in a directive")))
	    (recur (subs rest 1) (assoc flags flag true))))))))

(defn check-flags [def flags]
  (let [allowed (:flags def)]
    (if (and (not (:at allowed)) (:at flags))
      (throw (new Exception 
		  (str "@ is an illegal flag for format directive \"" (:directive def) "\""))))
    (if (and (not (:colon allowed)) (:colon flags))
      (throw (new Exception 
		  (str ": is an illegal flag for format directive \"" (:directive def) "\""))))
    (if (and (not (:both allowed)) (:at flags) (:colon flags))
      (throw (new Exception 
		  (str "Cannot combine @ and : flags for format directive \"" (:directive def) "\""))))))

(defn map-params [def params flags]
  "Takes a directive definition and the list of actual parameters and
   a map of flags and returns a map of the parameters and flags with defaults
   filled in. We check to make sure that there are the right types and number
   of parameters as well."
  (check-flags def flags)
  (cond 
   (> (count params) (count (:params def)))
   (throw (new Exception (str "Too many parameters for directive \"" (:directive def) 
			      "\": specified " (count params) " but received " 
			      (count (:params def)))))

   (filter not (map #(instance? (frest (frest %2)) %1) params (:params def)))
   (throw (new Exception (str "Bad parameter type for directive \"" (:directive def) "\""))) 
   
   true
   (merge ; create the result map
    (into {} (for [[name [default]] (:params def)] [name default])) ; start with the default values
    (zipmap (keys (:params def)) params) ; add the specified parameters
    flags)) ; and finally add the flags
)

;; TODO helpful error handling
(defn compile-directive [s]
  (let [[raw-params rest] (extract-params s)
	[flags rest] (extract-flags rest)
	directive (nth rest 0)
	def (get directive-table directive)
	params (map-params def (map translate-param raw-params) flags)
	]
    [[ ((:generator-fn def) params) directive params] (subs rest 1)]))
    
(defn breakup-string [ s ]
  (loop [ m (re-matcher directive-pattern s)
	 start 0
	 directive (re-find m)
	 acc []
	 ]
    (if (not directive)
      (concat acc [ (subs s start) ])
      (let [ prestr (subs s start (.start m)) 
	    this-iter (if (> (.length prestr) 0) [ prestr directive] [ directive ]) ]
	(recur m (. m end) (re-find m) (concat acc this-iter))))))

(defn compile-format [ format-str ]
;; TODO: Implement
)

(defn execute-format [stream format args]
;; TODO: Implement
)

