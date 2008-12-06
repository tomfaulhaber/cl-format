(ns format-tests
  (:refer-clojure :exclude [format])
  (:use unit-test format))

(def format cl-format)

(defn integers
  ([] (integers 1))
  ([n] (lazy-cons n (integers (inc n)))))

(defn group [unit lis]
  (first (consume (fn [l] [ (take unit l) (drop unit l)]) lis)))

(defmacro simple-tests [prefix & body]
  (cons 'do 
	(let [pairs (group 2 body)]
	  (map (fn [[statement expected-result] test-num] 
		 `(deftest ~(symbol (str prefix "-" test-num)) []
		    (assert-equal ~expected-result ~statement)))
	       pairs (integers)))))


(simple-tests square-bracket-tests
  (cl-format nil "I ~[don't ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~]have one~%" 1) "I have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 1) "I do have one\n"
  (cl-format nil "I ~[don't ~;do ~]have one~%" 2) "I have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 0) "I don't have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 1) "I do have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 2) "I do have one\n"
  (cl-format nil "I ~[don't ~:;do ~]have one~%" 700) "I do have one\n"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following tests are the various examples from the format
;; documentation in Common Lisp, the Language, 2nd edition, Chapter 22.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expt [base pow] (reduce * (replicate pow base)))

(let [x 5, y "elephant", n 3]
  (simple-tests cltl-intro-tests
   (format nil "foo")  "foo" 
   (format nil "The answer is ~D." x)  "The answer is 5." 
   (format nil "The answer is ~3D." x)  "The answer is   5." 
   (format nil "The answer is ~3,'0D." x)  "The answer is 005." 
   (format nil "The answer is ~:D." (expt 47 x)) "The answer is 229,345,007."
   (format nil "Look at the ~A!" y)  "Look at the elephant!" 
   (format nil "Type ~:C to ~A." (char 4) "delete all your files") 
   "Type Control-D to delete all your files."
   (format nil "~D item~:P found." n)  "3 items found."
   (format nil "~R dog~:[s are~; is~] here." n (= n 1)) "three dogs are here."
   (format nil "~R dog~:*~[s are~; is~:;s are~] here." n) "three dogs are here."
   (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n) "Here are three puppies."))
 
;; how do we do hex numbers??
;; (simple-tests cltl-B-tests
;;   (format nil "~,,' ,4B" #xFACE) "1111 1010 1100 1110" 
;;   (format nil "~,,' ,4B" #x1CE) "1 1100 1110" 
;;   (format nil "~19,,' ,4B" #xFACE) "1111 1010 1100 1110" 
;;   (format nil "~19,,' ,4B" #x1CE) "0000 0001 1100 1110")

(simple-tests cltl-P-tests
  (format nil "~D tr~:@P/~D win~:P" 7 1) "7 tries/1 win" 
  (format nil "~D tr~:@P/~D win~:P" 1 0) "1 try/0 wins" 
  (format nil "~D tr~:@P/~D win~:P" 1 3) "1 try/3 wins")

(defn foo [x] 
  (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F" 
          x x x x x x))

(simple-tests cltl-F-tests
  (foo 3.14159)  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159" 
  (foo -3.14159) " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159" 
  (foo 100.0)    "100.00|******|100.00| 100.0|100.00|100.0" 
  (foo 1234.0)   "1234.00|******|??????|1234.0|1234.00|1234.0" 
  (foo 0.006)    "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(defn foo-e [x] 
  (format nil 
          "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@E|~9,2E" 
          x x x x)) 

(simple-tests cltl-E-tests
  (foo-e 3.14159)  "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0" 
  (foo-e -3.14159) " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0" 
  (foo-e 1100.0)   "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3" 
;  (foo-e 1100.0L0) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3" 
  (foo-e 1.1E13)   "*********| 11.00$+12|+.001E+16| 1.10E+13" 
;  (foo-e 1.1L120)  "*********|??????????|%%%%%%%%%|1.10L+120" 
;  (foo-e 1.1L1200) "*********|??????????|%%%%%%%%%|1.10L+1200"
)

(defn foo-g [x] 
  (format nil 
          "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G" 
          x x x)) 

(simple-tests cltl-G-tests
  (foo-g 0.0314159) "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2" 
  (foo-g 0.314159)  "  0.31   |0.314    |0.314    | 0.31    " 
  (foo-g 3.14159)   "   3.1   | 3.14    | 3.14    |  3.1    " 
  (foo-g 31.4159)   "   31.   | 31.4    | 31.4    |  31.    " 
  (foo-g 314.159)   "  3.14E+2| 314.    | 314.    |  3.14E+2" 
  (foo-g 3141.59)   "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3" 
;  (foo-g 3141.59L0) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3" 
  (foo-g 3.14E12)   "*********|314.0$+10|0.314E+13| 3.14E+12" 
;  (foo-g 3.14L120)  "*********|?????????|%%%%%%%%%|3.14L+120" 
;  (foo-g 3.14L1200) "*********|?????????|%%%%%%%%%|3.14L+1200"
)

(defn type-clash-error [fun nargs argnum right-type wrong-type]
  (format nil
          "~&Function ~S requires its ~:[~:R~;~*~] ~ 
           argument to be of type ~S,~%but it was called ~ 
           with an argument of type ~S.~%" 
          fun (= nargs 1) argnum right-type wrong-type)) 

(simple-tests cltl-B-tests
  (type-clash-error 'aref nil 2 'integer 'vector)
"Function AREF requires its second argument to be of type INTEGER, 
but it was called with an argument of type VECTOR."
  (type-clash-error 'car 1 1 'list 'short-float)
"Function CAR requires its argument to be of type LIST, 
but it was called with an argument of type SHORT-FLOAT."
)

(simple-tests cltl-?-tests
  (format nil "~? ~D" "<~A ~D>" '("Foo" 5) 7) "<Foo 5> 7" 
  (format nil "~? ~D" "<~A ~D>" '("Foo" 5 14) 7) "<Foo 5> 7"
  (format nil "~@? ~D" "<~A ~D>" "Foo" 5 7) "<Foo 5> 7" 
  (format nil "~@? ~D" "<~A ~D>" "Foo" 5 14 7) "<Foo 5> 14")

(defn f [n] (format nil "~@(~R~) error~:P detected." n)) 

(simple-tests cltl-paren-tests
  (format nil "~@R ~(~@R~)" 14 14) "XIV xiv" 
  (f 0) "Zero errors detected." 
  (f 1) "One error detected." 
  (f 23) "Twenty-three errors detected.")

(let [*print-level* nil *print-length* 5] 
  (simple-tests cltl-bracket-tests
    (format nil "~@[ print level = ~D~]~@[ print length = ~D~]" 
            *print-level* *print-length*) 
    " print length = 5"))

(let [foo "Items:~#[ none~; ~S~; ~S and ~S~ 
           ~:;~@{~#[~; and~]
~S~^,~}~]."]
  (simple-tests cltl-bracket1-tests
    (format nil foo) "Items: none." 
    (format nil foo 'foo) "Items: FOO." 
    (format nil foo 'foo 'bar) "Items: FOO and BAR." 
    (format nil foo 'foo 'bar 'baz) "Items: FOO, BAR, and BAZ." 
    (format nil foo 'foo 'bar 'baz 'quux) "Items: FOO, BAR, BAZ, and QUUX."))

(simple-tests cltl-curly-bracket-tests
  (format nil 
        "The winners are:~{ ~S~}." 
        '(fred harry jill)) 
  "The winners are: FRED HARRY JILL." 

  (format nil "Pairs:~{ <~S,~S>~}." '(a 1 b 2 c 3)) 
  "Pairs: <A,1> <B,2> <C,3>."

  (format nil "Pairs:~:{ <~S,~S>~}." '((a 1) (b 2) (c 3))) 
  "Pairs: <A,1> <B,2> <C,3>."

  (format nil "Pairs:~@{ <~S,~S>~}." 'a 1 'b 2 'c 3) 
  "Pairs: <A,1> <B,2> <C,3>."

  (format nil "Pairs:~:@{ <~S,~S>~}." '(a 1) '(b 2) '(c 3)) 
  "Pairs: <A,1> <B,2> <C,3>.")

(simple-tests cltl-angle-bracket-tests
  (format nil "~10<foo~;bar~>")           "foo    bar" 
  (format nil "~10:<foo~;bar~>")          "  foo  bar" 
  (format nil "~10:@<foo~;bar~>")         "  foo bar " 
  (format nil "~10<foobar~>")             "    foobar" 
  (format nil "~10:<foobar~>")            "    foobar" 
  (format nil "~10@<foobar~>")            "foobar    " 
  (format nil "~10:@<foobar~>")           "  foobar  ")

(let [donestr "Done.~^  ~D warning~:P.~^  ~D error~:P."
      tellstr "~@(~@[~R~]~^ ~A.~)"]
  (simple-tests cltl-up-tests
    (format nil donestr) "Done." 
    (format nil donestr 3) "Done.  3 warnings." 
    (format nil donestr 1 5) "Done.  1 warning.  5 errors."
    (format nil tellstr 23) "Twenty-three." 
    (format nil tellstr nil "losers") "Losers." 
    (format nil tellstr 23 "losers") "Twenty-three losers."
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo) 
    "            FOO" 
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar) 
    "FOO         BAR" 
    (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz) 
    "FOO   BAR   BAZ"))

(simple-tests cltl-up-x3j13-tests
  (format nil 
	  "~:{/~S~^ ...~}" 
	  '((hot dog) (hamburger) (ice cream) (french fries))) 
  "/HOT .../HAMBURGER/ICE .../FRENCH ..."
  (format nil 
	  "~:{/~S~:^ ...~}" 
	  '((hot dog) (hamburger) (ice cream) (french fries))) 
  "/HOT .../HAMBURGER .../ICE .../FRENCH"

  (format nil 
	  "~:{/~S~:#^ ...~}" 
	  '((hot dog) (hamburger) (ice cream) (french fries))) 
  "/HOT .../HAMBURGER")

