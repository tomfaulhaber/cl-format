## Common Lisp Format for Clojure ##

**Note:** Most development is now happening on the pretty-printer in the pp branch. The 
master branch represents the more stable universe of things that I believe work.
 
cl-format is an implementation of the incredibly baroque Common Lisp format function as specified 
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

cl-format is 100% compatible with the Common Lisp standard as
specified in CLtLv2 with the exception of the support directives 
for the pretty printer.
This includes all of the functionality of Common
Lisp's format function including iteration, conditionals, 
text justification and rich
options for displaying real and integer values. 

If you find a bug in a directive, drop me a line
with a chunk of code that exhibits the bug and the version of
cl-format you found it in and I'll try to get it fixed.

I also intend to have good built-in documentation for the directives,
but I haven't built that yet.

Pretty printing isn't supported (at least at the moment) and with it,
the following directives specified as part of the X3J13 updates are
not supported: ~:T and ~@:T (but all other forms of ~T work), ~_,
~I, and extensions with ~/. ~W is supported, but without any pretty
printing functionality.

I'm still considering what to do about the pretty printer. CL defines
a super-powerful pretty printer based on the format function. But it's 
because it has more functions and variables, it interacts with the
rest of the system more and will, therefore, be harder to implement
with 100% compatibility.

Next up: 

* Support for the CL formatter function
* Import tests from CLISP and SBCL.
* Unit tests for exception conditions.
* Interactive documentation
* Think about pretty printing
 
### How to use cl-format ###

#### Installing using the JAR ####

The easiest way to install cl-format is just to grab the the jar from
github: 
[http://github.com/tomfaulhaber/cl-format/raw/master/cl-format.jar](http://github.com/tomfaulhaber/cl-format/raw/master/cl-format.jar). This
is not always the exact latest, but it does get updated when there are
significant changes. 

Add the jar to your classpath and proceed to the section *Loading
cl-format in your program* below to start using it. (I use the
clojure-extras shell script which automatically adds the JAR files in
my ~/.clojure directory, so I just copy cl-format.jar there.)

#### Use the source, Luke ####

If you'd like to start from the source, here's how.

Building and installing the library is pretty easy. There are a few
requirements, which you have probably already met if you're running
Clojure:

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
its result as a string rather than writing it to a stream.

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
 
### Using column aware streams across format invocations ### 

Writers in Java have no real idea of current column or device page width, so the format
directives that want to work relative to the current position on the
page have nothing to work with. To deal with this, cl-format contains
an extension to writer called ColumnWriter. ColumnWriter watches the
output and keeps track of what column the current output is going to.

When you call format and your format includes a directive that cares
about what column it's in (~T, ~&, ~<...~>), cl-format will
automatically wrap the Writer you passed in with a ColumnWriter. This
means that by default all cl-format statements act like they begin on
a fresh line and have a page width of 72.

For many applications, these assumptions are fine and you need to do
nothing more. But sometimes you want to use multiple cl-format calls
that output partial lines. You may also want to mix cl-format calls
with the native clojure calls like print. If you want stay
column-aware while doingg this you need to create a ColumnWriter of
your own (and possibly bind it to *out*).

As an example of this, this function takes a nested list and prints it
as a table (returning the result as a string):

    (defn list-to-table [aseq column-width]
      (let [stream (ColumnWriter. (java.io.StringWriter.))]
        (binding [*out* stream]
         (doseq [row aseq]
           (doseq [col row]
             (cl-format true "~4D~7,vT" col column-width))
           (prn)))
        (.toString (.getWriter stream))))

(In reality, you'd probably do this as a single call to cl-format.)

The constructor to ColumnWriter takes the Writer it's wrapping and
(optionally) the page width (in columns) for use with ~<...~>. 

### Examples ###

The following function uses cl-format to dump a columnized table of the Java system properties:

    (defn show-props [stream]
      (let [p (mapcat 
    	       #(vector (key %) (val %)) 
    	       (sort-by key (System/getProperties)))]
        (cl-format stream "~30A~A~%~{~20,,,'-A~10A~}~%~{~30A~S~%~}" 
    	           "Property" "Value" ["" "" "" ""] p)))
    
There are some more examples in the com.infolace.format.examples
package:

* hexdump - a program that uses cl-format to create a standard
formatted hexdump of the requested stream.
* multiply - a function to show a formatted multipication table in a
very "first-order" way.
* props - the show-props example shown above.
* show_doc - some utilities for showing what names are in various name spaces.

### Differences from the Common Lisp format function ###

The floating point directives that show exponents (~E, ~G) show E for
the exponent character in all cases (unless overridden with an
*exponentchar*).  Clojure does not distinguish between floats and
doubles in its printed representation and neither does cl-format.

The ~A and ~S directives accept the colon prefix, but ignore it since
() and nil are not equivalent in Clojure.

Clojure has 3 different reader syntaxes for characters. The ~@c
directive to cl-format has an argument extension to let youu choose:

* ~@c (with no argument) prints "\c" (backslash followed by the
printed representation of the character or \newline, \space, \tab,
\backspace, \return)
* ~'o@c prints "\oDDD" where DDD are the octal digits representing the
character. 
* ~'u@c prints "\uXXXX" prints the hex Unicode representation of the
character.  
