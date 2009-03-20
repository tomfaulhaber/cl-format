;   Copyright (c) Tom Faulhaber, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.format.test.benchmark
  (:use com.infolace.format))


(defn pprint-file [f]
  (with-open [r (java.io.PushbackReader. 
                 (java.io.FileReader. f))]
    (binding [*print-pprint-dispatch* *code-dispatch*
              *print-suppress-namespaces* true] 
      (loop [form (read r false :eof-special-token)]
        (when (not= form :eof-special-token) 
          (if (not (= (first form) 'defmacro)) ; skip the macros (and backquotes) for now
            (pprint form))
          (recur (read r false :eof-special-token)))))))

(defn do-timing [iters]
  (time
   (with-out-str 
     (dotimes [_ iters] 
       (pprint-file "com/infolace/format_base.clj"))))
  nil)

(defn explode-base [] 
  (pprint (with-in-str "(defmacro formatter
  [format-in]
  `(let [compiled-format# (if (string? ~format-in) (compile-format ~format-in) ~format-in)
         func# (fn [stream# & args#]
                 (let [navigator# (init-navigator args#)]
                   (execute-format stream# compiled-format# navigator#)))]
     func#))
" (read))))

(defn explode [] 
  (binding [*print-pprint-dispatch* *code-dispatch*
              *print-suppress-namespaces* true]
;;    (pprint (with-in-str "`([([(init-navigator)])])" 
    (pprint (with-in-str "`[[[[[init-navigator]]]]]" 
              (read)))))
