;; unit-test.clj
;;
;; Copyright (c) Shane Celis. 
;;
;; The use and distribution terms for this software are covered by the
;; Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; This library is an xUnit unit testing framework for Clojure.

(ns unit-test)

(def #^{:doc "Ignore stack traces from these patterns"}ignore-files 
     [".*\\.java" "swank-clojure\\.clj" "boot\\.clj" "unit_test\\.clj"])

(defmacro deftest
  "Defines a test by adding a :unit-test tag to the function's meta data."  
  [name parameters & body]
  `(defn ~name {:unit-test true} ~parameters
       ~@body))

(defstruct #^{:doc "Stores the tests executed; failures and
  errors are lists of throwables."}
  test-result :tests :failures :errors)

(defn init-test-result 
  "Returns an initialized test-result map."
  [] (struct-map test-result :tests '() :failures '() :errors '()))

(defn fail 
  "Fail a test returning a message."
  ([] (throw (new java.lang.AssertionError "Assert failed!")))
  ([& msgs] (throw (new java.lang.AssertionError (apply str msgs)))))

(defmacro assert-equal 
  "Passes if expected is equal to actual."
  [expected actual & msgs]
  `(let [e# ~expected
         a# ~actual]
     (when-not (= e# a#)
       (fail "Expected: " (pr-str e#) " but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-not-equal 
  "Passes if expected is not equal to actual."
  [expected actual & msgs]
  `(let [e# ~expected
         a# ~actual]
     (when-not (not= e# a#)
       (fail "Did not expect: " (pr-str e#) " but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-same 
  "Passes if expected is identical to actual."
  [expected actual & msgs]
  `(let [e# ~expected
         a# ~actual]
     (when-not (identical? e# a#)
       (fail "Expected same: " (pr-str e#) " but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-not-same 
  "Passes if expected is not identical to actual."
  [expected actual & msgs]
  `(let [e# ~expected
         a# ~actual]
     (when-not (not (identical? e# a#))
       (fail "Did not expect same: " (pr-str e#) " but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-nil 
  "Passes if actual is nil."
  [actual & msgs]
  `(let [a# ~actual]
     (when-not (nil? a#)
       (fail "Expected nil but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-not-nil 
  "Passes if actual is not nil."
  [actual & msgs]
  `(let [a# ~actual]
     (when-not (not (nil? a#))
       (fail "Expected not nil but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-true 
  "Passes if actual is true."
  [actual & msgs]
  `(let [a# ~actual]
     (when-not a#
       (fail "Expected true but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-not-true 
  "Passes if actual is not true (synonym for assert-false)."
  [actual & msgs]
  `(let [a# ~actual]
     (when-not (not a#)
       (fail "Expected not true but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-false 
  "Passes if actual is false (synonym for assert-not-true)."
  [actual & msgs]
  `(let [a# ~actual]
     (when-not (not a#)
       (fail "Expected false but was: " (pr-str a#) " <- " (pr-str '~actual) " " ~@msgs))))

(defmacro assert-expect 
  "Passes if the expr produces the expected exception."
  ([exception expr] `(assert-expect ~exception ~expr "Did not get expected exception " ~exception))
  ([exception expr & msgs]
  `(let [v# (try
             ~expr
             (catch ~exception e#
               ;;good
               :caught-one
               ))]
     (when-not (= v# :caught-one)
       (fail ~@msgs)))))

(defmacro assert-fail 
  "Passes if the expr produces a failure."
  [expr & msgs]
  `(assert-expect java.lang.AssertionError ~expr ~@msgs))

(defmacro assert-fail-each 
  "Passes if all expressions produce a failure."
  [& exprs]
  `(do ~@(map (partial list 'assert-fail) exprs)))

;; (defmacro defassert [name args hash]
;;   `(defmacro ~name ~args
;;        (when-not (apply (fn ~args (:test ~hash)) ~args)
;;          (fail (apply (fn ~args (:msg ~hash)) ~args)))))

(defn rpartial 
  "Like partial but arguments are appended from the end not the beginning."
  [f & x]
  (fn [& y]
    (apply f (concat y x))))

(defn all-tests 
  "Returns all the tests in the given namespace or in the current namespace."
  ([] (all-tests (ns-name *ns*)))
  ([ns-sym] 
     (let [ns (or (find-ns ns-sym) (throw (new Exception "No such namespace")))
           fns   (sort-by #(:name (meta %)) (map second (ns-interns ns)))
           metas (map meta fns)
           tests (filter (comp (rpartial contains? :unit-test) second) 
                         (map list fns metas))]
       (map first tests))))

;; (defn mapn [f & xs]
;;   (loop [ys xs]
;;     (when-not (some nil? ys)
;;       (apply f (map first ys))
;;       (recur (map rest ys)))))

(defn to-list 
  "Converts a Java array to a list."
  [arr]
  (map (partial aget arr) (range (alength arr))))

;; ah, didn't see interpose.
;(defn join-with [delim list]
;  (apply str (rest (interleave (repeat delim) list))))

(defn filter-stacktrace 
  "Filters a stacktrace removing file patterns defined in ignore-files."
  [t]
  (let [es (to-list (. t getStackTrace))
	top (take-while #(not (.contains (.getClassName %) "unit_test$run_test__")) es)
        cljs (filter (comp not nil? (memfn getFileName)) top)
        ignore-regex (str "(" (apply str (interpose "|" ignore-files)) ")")
        ignore-pattern (re-pattern ignore-regex)
        junk (filter (comp not 
                           (partial re-matches ignore-pattern) 
                           (memfn getFileName)) cljs)
        ]
    junk))

(defn assoc-append 
  "Appends an element to a map's list for a particular key."
  [map key val]
  (assoc map key (cons val (map key))))

(defn show-stacktraces 
  "Shows a stack trace that has been filtered to hopefully show only the useful information."
  [failures]
  (let [f (fn [[name failure] n]
            (println (str (inc n) ") " name ": " \newline) 
                     (if (instance? java.lang.AssertionError failure)
                       (. failure getMessage)
                       (. failure toString)))
            (doall (map (comp (partial println "   ") (memfn toString))
			(filter-stacktrace failure))))]
    (doall (map f failures (range (count failures))))
    ))

(defn show-test-result 
  "Prints the contents of a test result in human readable format."
  [tresult]
  (let [tests (:tests tresult)
        failures (:failures tresult)
        errors (:errors tresult)]
    (newline)
    (println "Tests run" (count tests) 
             "failures" (count failures) 
             "errors" (count errors))
    (when (> (count failures) 0)
      (println "Failures:")
      (show-stacktraces (reverse failures)))
    (when (> (count errors) 0)
      (println "Errors:")
      (show-stacktraces (reverse errors)))
    ))

(defn run-test 
  "Runs one test and appends the results to the given test-result
or a new test-result."
  ([test] (run-test test (init-test-result)))
  ([test test-result]
     (let [r (try 
              (do (print ".")
                  (test)
                  test-result)
              (catch java.lang.AssertionError a
                (print "F")
                (assoc-append test-result :failures [(:name (meta test)) a])
                )
              (catch Throwable t
                (print "E")
                (assoc-append test-result :errors [(:name (meta test)) t])
                )
              )]
       (assoc-append r :tests test))))

(defn my-split-with 
  "Splits a collection exhaustively into a [matching not-matching] vector."
  [pred coll]
  [(filter pred coll) (filter (comp not pred) coll)])

(defn run-tests 
  "Runs the given tests or runs all the tests given from (all-tests)"
  ([] (run-tests (all-tests)))
  ([tests]
     (let [tresult (reduce #(run-test %2 %1) (init-test-result) tests)]
       (show-test-result tresult)
       (flush))))

(defn run-all-tests 
  "Runs all the tests in the given or current namespace."
  ([] (run-all-tests (ns-name *ns*)))
  ([ns-sym] (run-tests (all-tests ns-sym))))

(defn exec-tests 
  "Runs the tests and returns the test result without any printing."
  ([] (exec-tests (all-tests)))
  ([tests] 
     (binding [print (fn [& x])]
       (reduce #(run-test %2 %1) (init-test-result) tests)
       )))
