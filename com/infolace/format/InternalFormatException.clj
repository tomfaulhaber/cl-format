(ns com.infolace.format.InternalFormatException
  (:gen-class
   :extends Exception
   :init init
   :constructors { [ String Integer ] [ String ] }
   :state pos))

(defn- -init [msg level]
  [[msg] level])

