## Common Lisp Format for Clojure ##

cl-format is a implementation of the incredibly baroque Common Lisp format function as specified 
in [*Common Lisp the Language*, 2nd edition, Chapter 22](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000).

Format gives you an easy and powerful way to format text and data for output. It supports rich 
formatting of strings and numbers, loops, conditionals, embedded formats, etc. It is really a 
domain-specific language for formatting.

This implementation for clojure has the following goals:

* Support the full feature set of the Common Lisp format function (including
the X3J13 extensions) with the only exception 
being concepts that make no sense or are differently interpreted in Clojure.
* Make porting code from Common Lisp easier.
* Provide a more native feeling solution for Clojure programmers than the Java format method and
its relatives.
* Be fast. This includes the ability to precompile formats that are going to be used reptitively.
* Include useful error handling and comprehensive documentation.

cl-format is being developed by Tom Faulhaber (to mail me you can use
my first name at my domain which is infolace.com).

cl-format is licensed under the 
[Eclipse Public License 1.0](http://opensource.org/licenses/eclipse-1.0.php)
(like Clojure itself)
which can be found in the file epl-v10.html at the root of this
distribution.

cl-format is hosted at github at 
[http://github.com/tomfaulhaber/cl-format](http://github.com/tomfaulhaber/cl-format/).

### Why would I use cl-format? ###

For some people the answer to this question is that they are used to
Common Lisp and, therefore, they already know the syntax of format
strings and all the directives.

A more interesting answer is that cl-format provides a way of
rendering strings that is much more suited to Lisp and its data
structures. 

Because iteration and conditionals are built into the directive
structure of cl-format, it is possible to render sequences and other
complex data structures directly without having to loop over the data
structure. 

For example, to print the elements of a sequence separated by commas,
you simply say:

    (cl-format true "~{~a~^, ~}" aseq)

(This example is taken from 
[Practical Common Lisp](http://www.gigamonkeys.com/book/)
 by Peter Seibel.)

The corresponding output using Clojure's Java-based *format* function
would involve a nasty loop/recur with some code to figure out about
the commas. Yuck!

### Current Status ###

cl-format currently supports most of the functionality of Common
Lisp's format function including iteration, conditionals, and rich
options for displaying real and integer values. What isn't done yet
is under active development and will be done soon (yes, even Roman
numerals).

If you have specific features you're anxious to use, contact me and
I'll give them priority.

If you find a bug in a directive that I say is supported, drop me a line
with a chunk of code that exhibits the bug and the version of
cl-format you found it in and I'll try to get it fixed.

I also intend to have good built-in documentation for the directives,
but I haven't built that yet.

The following list shows what directives it supports

    ~A: All except "~:A" which doesn't make sense in clojure
    ~S: All except "~:S" which doesn't make sense in clojure
    ~D: All
    ~B: *Not yet*
    ~O: All
    ~X: All
    ~R: *Not yet*
    ~P: All
    ~C: *Not yet*
    ~F: All
    ~E: All
    ~G: All
    ~$: All
    ~%: All
    ~&: *Not yet*
    ~|: *Not yet*
    ~~: All
    ~<newline>: *Not yet*
    ~T: *Not yet*
    ~*: All
    ~?: All
    ~_: *Not yet*
    ~W: *Not yet*
    ~I: *Not yet*
    ~(*str*~): *Not yet*
    ~[*str0*~;*str1*~;*...*~;*strn*~]: All
    ~{*str*~}: All
    ~<*str*~>: *Not yet*
    ~^: *Not yet*
    ~/: *Not yet*

Next up: Support for ~^ and ~R.
### How to use cl-format ###

#### Installation ####

To use the cl-format function, you must download and build the
library. There is currently no binary repository.

Building and installing the library is pretty easy. There are a few
requirements, which you have probably already met if you're running
clojure:

* The Java SDK, version 1.5 or later.
* The Ant build tool
* A built version of clojure.jar, the clojure library

Once you have these, follow these steps:

1. Download the cl-format sources. They are hosted on github. This
gives you two choices. You can either (1) just grab the latest tarball by
going to github @
[http://github.com/tomfaulhaber/cl-format](http://github.com/tomfaulhaber/cl-format/)
and clicking on the download link or (2) clone the git repository with
"git clone git://github.com/tomfaulhaber/cl-format.git"

2. If you started from a tarball, untar it into a directory and cd to
it. If you cloned the git repository, cd into that directory.

3. Edit build.xml so that the clojure-jar property points at the
clojure JAR file. The default is that it is in the clojure source
directory parallel to the cl-format directory.

4. Run "ant clean" and "ant" in the root directory of the
distribution. This should build the cl-format.jar library file.

5. Add the cl-format.jar file to your classpath. I use the
clojure-extras shell script which automatically adds the JAR files in
my ~/.clojure directory, so I just copy cl-format.jar there.

Now you're ready to format! See the next section for how to use
cl-format from a Clojure program.

#### Loading cl-format in your program ####

Once cl-format is in your path, adding it to your code is easy:

    (ns your-namespace-here
      (:use com.infolace.format))

If you want to refer to the cl-format function as "format" (rather
than using the clojure function of that name), you can use this idiom:

    (ns your-namespace-here
      (:refer-clojure :exclude [format])
      (:use com.infolace.format))

    (def format cl-format)

You might want to do this in code that you've ported from Common Lisp,
for instance, or maybe just because old habits die hard.

From the REPL, you can grab it using (require) and (refer):

    (require 'com.infolace.format)
    (refer 'com.infolace.format)

#### Calling cl-format ####

cl-format is a standard clojure function that takes a variable number
of arguments. You call it like this:

    (cl-format stream format args...)

*stream* can be any Java Writer (that is java.io.Writer) or the values
*true*, *false*, or *nil*. The argument *true* is identical to using
*out* while *false* or *nil* indicate that cl-format should return
it's result as a string rather than writing it to a stream.

*format* is either a format string or a compiled format (see
 below). The format string controls the output that's written in a way
 that's similar to (but much more powerful than) the standard Clojure
 API format function (which is based on Java's
 java.lang.String.Format).

Format strings consist of characters that are to be written to the
output stream plus directives (which are marked by ~) as in "The
answer is ~,2f". Format strings are documented in detail in 
[*Common Lisp the Language*, 2nd edition, Chapter 22](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000).

*args* is a set of arguments whose use is defined by the format.

#### Compiled formats ####

When you use a format string many times (for example, when you're outputting
in a loop), you can improve your performance by compiling the format
with *compile-format*. The result of compile format can be passed to
*cl-format* just like a format string but it doesn't need to be
parsed.

For example:

    (def log-format (compile-format "~2,'0D/~2,'0D/~D ~2D:~2,'0D ~:[PM,AM]: ~A~%"))

    (defn log [msg]
      (let [[m d y h min am?] (some-date-decomposition-fn)]
        (cl-format log-format m d y h min am? msg)))
 
### Examples ###

The following function uses cl-format to dump a columnized table of the Java system properties:

    (defn show-props [stream]
      (let [p (mapcat 
    	       #(vector (key %) (val %)) 
    	       (sort-by key (System/getProperties)))]
        (cl-format stream "~30A~A~%~{~20,,,'-A~10A~}~%~{~30A~S~%~}" 
    	           "Property" "Value" ["" "" "" ""] p)))
    

### Differences from the Common Lisp format function ###

The floating point directives that show exponents (~E, ~G) show E for
the exponent character in all cases (unless overridden with an
*exponentchar*).  Clojure does not distinguish between floats and
doubles in its printed representation and neither does cl-format.
