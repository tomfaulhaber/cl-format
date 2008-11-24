(ns format-cltl-tests
  (:refer-clojure :exclude [format])
  (:use unit-test format))

(def format cl-format)

(deftest simplest-examples []
  (let [y "elephant"]
    (assert-equal "Look at the elephant!" (cl-format nil "Look at the ~A!" y))))

(deftest intro-examples []
  (assert-equal "foo" (format nil "foo"))
  (let [x 5]
    (assert-equal "The answer is 5." (format nil "The answer is ~D." x))
    (assert-equal "The answer is   5." (format nil "The answer is ~3D." x))
    (assert-equal "The answer is 005." (format nil "The answer is ~3,'0D." x))
    (comment ;;; TODO: Clojure doesn't have expt figure out the right way
      (assert-equal "The answer is 229,345,007." (format nil "The answer is ~:D." (expt 47 x))))
    )
  (let [y "elephant"]
    (assert-equal "Look at the elephant!" (format nil "Look at the ~A!" y)))

  (assert-equal "Type Control-D to delete all your files."
		(format nil "Type ~:C to ~A." 
			(char 4) 
			"delete all your files") )

  (let [n 3]
    (assert-equal "3 items found." (format nil "~D item~:P found." n))
    (assert-equal "three dogs are here." 
		  (format nil "~R dog~:[s are~; is~] here." n (= n 1)))
    (assert-equal "three dogs are here."
		  (format nil "~R dog~:*~[s are~; is~:;s are~] here." n) )
    (assert-equal "Here are three puppies."
		  (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n) ))
)
