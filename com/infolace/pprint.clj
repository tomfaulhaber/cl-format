;   Copyright (c) Tom Faulhaber, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'com.infolace.format)

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
(defonce ; If folks have added stuff here, don't overwrite
 #^{ :doc "The pretty print dispatch table"}
 *print-pprint-dispatch* (ref []))

(def
 #^{ :doc "Pretty printing will try to avoid anything going beyond this column."}
 *print-right-margin* 72)

(def
 #^{ :doc "The column at which to enter miser style. Depending on the dispatch table, 
miser style add newlines in more places to try to keep lines short allowing for further 
levels of nesting."}
 *print-miser-width* 40)

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

(def
 #^{ :doc "Don't print namespaces with symbols. This is particularly useful when 
pretty printing the results of macro expansions"}
 *print-suppress-namespaces* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the write function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^{:private true} write-option-table
     {;:array            *print-array*
      ;;:base             *print-base*,
      ;;:case             *print-case*,
      :circle           'com.infolace.format/*print-circle*,
      ;;:escape           *print-escape*,
      ;;:gensym           *print-gensym*,
      :length           'clojure.core/*print-length*,
      :level            'clojure.core/*print-level*,
      :lines            'com.infolace.format/*print-lines*,
      :miser-width      'com.infolace.format/*print-miser-width*,
      :pprint-dispatch  'com.infolace.format/*print-pprint-dispatch*,
      :pretty           'com.infolace.format/*print-pretty*,
      ;;:radix            *print-radix*,
      :readably         'clojure.core/*print-readably*,
      :right-margin     'com.infolace.format/*print-right-margin*})


;; TODO: build a macro that only rebinds changed things (base it on the
;; implementation of "binding")
(defmacro #^{:private true} binding-map [symbol-map options & body]
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
;;                   (fn [[key val]] (if-let [var-name (key real-map)]
;;                                     `(~var-name ~val)))
;;                   options)]
;;        ~@body)))

(defn pretty-writer? [x] (instance? PrettyWriter x))
(defn make-pretty-writer [base-writer right-margin miser-width]
  (PrettyWriter. base-writer right-margin miser-width))

(defmacro #^{:private true} with-pretty-writer [base-writer & body]
  `(let [new-writer# (not (pretty-writer? ~base-writer))]
     (binding [*out* (if new-writer#
                      (make-pretty-writer ~base-writer *print-right-margin* *print-miser-width*)
                      ~base-writer)]
       ~@body
       (if new-writer# (.flush *out*)))))

(defn write 
  "Write an object subject to the current bindings of the printer control variables.
Use the options argument to override individual variables for this call (and any 
recursive calls). Returns the string result if :stream is nil or nil otherwise."
  [object & kw-args]
  (let [options (merge {:stream true} (apply hash-map kw-args))]
    (binding-map write-option-table options 
      (let [optval (if (contains? options :stream) 
                     (:stream options)
                     true) 
            base-writer (condp = optval
                          nil (java.io.StringWriter.)
                          true *out*
                          optval)]
        (if *print-pretty*
          (with-pretty-writer base-writer
            ;; TODO better/faster dispatch mechanism!
            (loop [dispatch @*print-pprint-dispatch*]
              (let [[test func] (first dispatch)]
                (cond
                  (empty? dispatch) (if (and *print-suppress-namespaces* (symbol? object))
                                      (print (name object))
                                      (pr object))
                  (test object) (func *out* object)
                  :else (recur (next dispatch))))))
          (binding [*out* base-writer]
            (pr object)))
        (if (nil? optval) 
          (.toString base-writer))))))

(defn pprint 
  "Pretty print object to the optional output writer. If the writer is not provided, 
print the object to the currently bound value of *out*."
  [object & more]
  (let [base-stream (if (pos? (count more))
                 (first more)
                 *out*)]
    (with-pretty-writer base-stream
      (write object :pretty true)
      (if (not (= 0 (.getColumn *out*)))
        (.write *out* (int \newline))))))

(defmacro pp 
  "A convenience macro that pretty prints the last thing output. This is
exactly equivalent to (pprint *1)."
  [] `(pprint *1))

(defn set-pprint-dispatch  
  "Set the pretty print dispatch table to TABLE. Currently the supported values are
*simple-dispatch* or *code-dispatch*. In the future, this will support custom tables."
  [table]
  (dosync (ref-set *print-pprint-dispatch* @table))
  nil)

(defmacro with-pprint-dispatch 
  "Execute body with the pretty print dispatch table bound to table."
  [table & body]
  `(binding [*print-pprint-dispatch* ~table]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the functional interface to the pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-lb-options [opts body]
  (loop [body body
         acc []]
    (if (opts (first body))
      (recur (drop 2 body) (concat acc (take 2 body)))
      [(apply hash-map acc) body])))

(defn- check-enumerated-arg [arg choices]
  (if-not (choices arg)
          (throw
           (IllegalArgumentException.
            ;; TODO clean up choices string
            (str "Bad argument: " arg ". It must be one of " choices)))))

(defmacro pprint-logical-block 
  "Execute the body as a pretty printing logical block with output to *out* which 
is a pretty printing writer wrapping base-stream (unless base-stream is already a pretty 
printing writer in which case *out* is just bound to base-stream). 

After the writer, the caller can optionally specify :prefix, :per-line-prefix, and
:suffix."
  [base-stream & body]
  (let [[options body] (parse-lb-options #{:prefix :per-line-prefix :suffix} body)]
    `(with-pretty-writer ~base-stream
       (.startBlock *out* ~(:prefix options) ~(:per-line-prefix options) ~(:suffix options))
       ~@body
       (.endBlock *out*)
       nil)))

(defn pprint-newline
  "Print a conditional newline to a pretty printing stream. kind specifies if the 
newline is :linear, :miser, :fill, or :mandatory. 

Optionally, a second argument which is a stream may be used. If supplied, that is 
the writer to which the newline is sent, otherwise *out* is used.

If the requested stream is not a PrettyWriter, this function does nothing."
  [kind & more] 
  (check-enumerated-arg kind #{:linear :miser :fill :mandatory})
  (let [stream (if (pos? (count more))
                 (first more)
                 *out*)]
    (if (instance? PrettyWriter stream)
      (.newline stream kind))))

(defn pprint-indent 
  "Create an indent at this point in the pretty printing stream. This defines how 
following lines are indented. relative-to can be either :block or :current depending 
whether the indent should be computed relative to the start of the logical block or
the current column position. n is an offset. 

Optionally, a third argument which is a stream may be used. If supplied, that is 
the writer indented, otherwise *out* is used.

If the requested stream is not a PrettyWriter, this function does nothing."
  [relative-to n & more] 
  (check-enumerated-arg relative-to #{:block :current})
  (let [stream (if (pos? (count more))
                 (first more)
                 *out*)]
    (if (instance? PrettyWriter stream)
      (.indent stream relative-to n))))

;; TODO a real implementation for pprint-tab
(defn pprint-tab 
  "Tab at this point in the pretty printing stream. kind specifies whether the tab
is :line, :section, :line-relative, or :section-relative. 

Colnum and colinc specify the target column and the increment to move the target
forward if the output is already past the original target.

Optionally, a fourth argument which is a stream may be used. If supplied, that is 
the writer indented, otherwise *out* is used.

If the requested stream is not a PrettyWriter, this function does nothing.

THIS FUNCTION IS NOT YET IMPLEMENTED."
  [kind colnum colinc & more] 
  (check-enumerated-arg kind #{:line :section :line-relative :section-relative})
  (let [stream (if (pos? (count more))
                 (first more)
                 *out*)]
    (if (instance? PrettyWriter stream)
      (throw (UnsupportedOperationException. "pprint-tab is not yet implemented")))))


nil
