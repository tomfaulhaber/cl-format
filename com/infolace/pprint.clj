;   Copyright (c) Tom Faulhaber, Jan 2009. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns com.infolace.pprint
  (:use com.infolace.format.utilities)
  (:import [com.infolace.format ColumnWriter]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that control the pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; *print-length*, *print-level* and *print-dup* are defined in clojure.core
;;; TODO: use *print-length*, *print-level* and *print-dup* here (or are they
;;; supplanted by other variables?)


(def
 #^{ :doc "Bind to true if you want write to use pretty printing"}
 *print-pretty* nil)

;;; TODO: implement true data-driven dispatch
(def
 #^{ :doc "The pretty print dispatch table (N.B. This is not yet used)"}
 *print-pprint-dispatch* nil)

(def
 #^{ :doc "Pretty printing will try to avoid anything going beyond this column."}
 *print-right-margin* nil)

;;; TODO implement miser style
(def
 #^{ :doc "The column at which to enter miser style (N.B. This is not yet used)"}
 *print-miser-width* nil)

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


(defmacro pprint-logical-block 
  ""
  [stream-sym arg-list options & body])
