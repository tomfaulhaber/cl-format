## Pretty Printing and Format for Clojure ##

**Note:** The current version of cl-format requires the lazy version of Clojure
which, as of this writing, is only available directly from the
subversion repository. If you are running on an older version of
Clojure, grab the v1.0.1 tag from the git repository. However, the
older version doesn't support pretty printing.

### Overview ###

This library adds two new features to Clojure: a generalized pretty
printer and a Common Lisp-compatible format function.

The pretty printer is easy to use:

    user=> (println (for [x (range 10)] (range x)))
    (() (0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4) (0 1 2 3 4 5) (0 1 2 3 4 5 6) (0 1 2 3 4 5 6 7) (0 1 2 3 4 5 6 7 8))
    nil
    user=> (use 'com.infolace.format)             
    nil
    user=> (pprint (for [x (range 10)] (range x)))         
    (()
     (0)
     (0 1)
     (0 1 2)
     (0 1 2 3)
     (0 1 2 3 4)
     (0 1 2 3 4 5)
     (0 1 2 3 4 5 6)
     (0 1 2 3 4 5 6 7)
     (0 1 2 3 4 5 6 7 8))
    nil
    user=>

The pretty printer supports two modes: *code* which has special
formatting for special forms and core macros and *simple* (the
default) which formats the various Clojure data structures as
appropriate for raw data. In the future, the pretty printer will be
highly customizable, but right now it is pretty simple.

The Common Lisp-compatible format function is a 100% comptible
implementation of format from Common Lisp (the one incompatibility is
that it's called cl-format in Clojure because format existed and meant
something different).

All the functions and variables described here are in the
com.infolace.format namespace. Using them is as simple as adding
cl-format.jar to your classpath and adding a (:use com.infolace.format) to
your namespace declarations.

cl-format is being developed by Tom Faulhaber (to mail me you can use
my first name at my domain which is infolace.com).

cl-format has been developed completely from scratch and is licensed under the 
[Eclipse Public License 1.0](http://opensource.org/licenses/eclipse-1.0.php)
(like Clojure itself)
which can be found in the file epl-v10.html at the root of this
distribution. This means it can go anywhere Clojure can go.

cl-format is hosted at github at 
[http://github.com/tomfaulhaber/cl-format](http://github.com/tomfaulhaber/cl-format/).

You can just grab the jar file here:
[http://github.com/tomfaulhaber/cl-format/raw/master/release/cl-format.jar](http://github.com/tomfaulhaber/cl-format/raw/master/release/cl-format.jar)

Future development is guided by those using it, so send feedback about
what's working and not working for you and what you'd like to see in cl-format.

### Pretty Printing ###

Pretty printing is primarily implemented with the function
pprint. pprint takes a single argument and formats it according to the
settings of several special variables.

Generally, the defaults are fine for pretty printing and you can
simply use:

    (pprint obj)

to print your object. If you wish to write to
another stream besides \*out\*, you can use:

    (write obj :pretty true :stream foo)

where foo is the stream to which you wish to write. (The write
function has a lot more options which are not yet documented. Stay
tuned.)

When at the REPL, the pp macro pretty prints the last output
value. This is useful when you get something too complex to read
comfortably. Just type:

    user=> (pp)

and you'll get a pretty printed version of the last thing output (the
magic variable *1).

#### Dispatch tables and code formatting ####

The behavior of the pretty printer can be finely controlled through
the use of *dispatch tables* that contain descriptions for how
different structures should be formatted. The exact design of the
dispatch table is still evolving and is, therefore, not yet
documented.

The pretty printer comes with two pre-defined dispatch tables to cover
the most common situations:

\*simple-dispatch\* - supports basic representation of data in various
Clojure structures: seqs, maps, vectors, etc. in a fairly statndard
way. When structures need to be broken across lines, following lines
are indented to line up with the first element. \*simple-dispatch\* is
the default and is good from showing the output of most operations.

\*code-dispatch\* - has special representation for various structures
found in code: defn, condp, binding vectors, anonymous functions,
etc. This dispatch indents following lines of a list one more space as
appropriate for a function/argument type of list.

An example formatted with code dispatch:

    user=> (def code '(defn cl-format 
    "An implementation of a Common Lisp compatible format function"
    [stream format-in & args] (let [compiled-format (if (string? format-in) 
    (compile-format format-in) format-in) navigator (init-navigator args)] 
    (execute-format stream compiled-format navigator))))
    #'user/code
    user=> (with-pprint-dispatch *code-dispatch* (pprint code))
    (defn cl-format
      "An implementation of a Common Lisp compatible format function"
      [stream format-in & args]
      (let [compiled-format (if (string? format-in)
                              (compile-format format-in)
                              format-in)
            navigator (init-navigator args)]
        (execute-format stream compiled-format navigator)))
    nil
    user=> 

There are two ways to set the current dispatch: set it to a specific
table permanantly with set-pprint-dispatch or bind it with
with-pprint-dispatch (as shown in the example above).

#### Control variables ####

The operation of pretty printing is also controlled by a set of variables
that control general parameters of how the pretty printer makes
decisions. The current list is as follows:

**\*print-pretty\***: Default: **true**  

Bind to true if you want write to use pretty printing. (pprint and pp automatically 
bind this to true.)

**\*print-right-margin\***: Default: **72**

Pretty printing will try to avoid anything going beyond this column.

**\*print-miser-width\***: Default: **40**

The column at which to enter miser style. Depending on the dispatch table, 
miser style add newlines in more places to try to keep lines short allowing for further 
levels of nesting. For example, in the code dispatch table, the pretty printer will 
insert a newline between the "if" and its condition when in miser style.

**\*print-suppress-namespaces\***: Default: **false**

Don't print namespaces with symbols. This is particularly useful when 
pretty printing the results of macro expansions

**\*print-level\***: Default: **nil**

As with the regular Clojure print function, this variable controls the 
depth of structure that is printed. The argument itself is level 0,
the first level of a collection is level 1, etc. When the structure
gets deeper than the specified \*print-level\*, a hash sign (#) is
printed.

For example:

    user=> (binding [*print-level* 2] (pprint '(a b (c d) ((e) ((f d) g)))))
    (a b (c d) (# #))
    nil
    user=> 

**\*print-length\***: Default: **nil**

As with the regular Clojure print function, this variable controls the 
number of items that are printed at each layer of structure. When a
layer has too many items, elipses (...) are displayed. 

For example:

    user=> (defn foo [x] (for [i (range x) ] (range 1 (- x (dec i)))))
    #'user/foo
    user=> (binding [*print-length* 6] (pprint (foo 10)))
    ((1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6)
     (1 2 3 4 5)
     ...)
    nil
    user=>

#### Current limitations and future plans ####

This is an early version release of the pretty printer and there is
plenty that is yet to come.

Here are some examples:

* Support all the types and forms in Clojure (most of the way there now).
* Customized pretty printing functions and dispatch tables. (They are
there under the hood, but the implementation is still evolving.)
* Support for limiting pretty printing based on line counts.
* Support for circular and shared substructure detection.
* Finishing the integration with the format function (support for ~/
and tabular pretty printing).
* Performance! (Not much thought has been made to making this go fast,
but there are a bunch of pretty obvious speedups to be had.)
* Handle Java objects intelligently

Please let me know about anything that's not working right, anything that
should work differently, or the feature you think should be at the top
of my list. 

### Common Lisp-compatible Format function ###
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

### Current Status of cl-format ###

cl-format is 100% compatible with the Common Lisp standard as
specified in CLtLv2.
This includes all of the functionality of Common
Lisp's format function including iteration, conditionals, 
text justification and rich
options for displaying real and integer values. It also includes the
directives to support pretty printing structured output.

If you find a bug in a directive, drop me a line
with a chunk of code that exhibits the bug and the version of
cl-format you found it in and I'll try to get it fixed.

I also intend to have good built-in documentation for the directives,
but I haven't built that yet.

The following directives are
not yet supported: ~:T and ~@:T (but all other forms of ~T work) 
and extensions with ~/. 

The pretty printer interface is similar, but not identical to the 
interface in Common Lisp.

The custom dispatch table functionality is not fully fleshed out yet.

Next up: 

* Debug pretty printer functionality
* Support for ~/
* Build up standard dispatch tables
* Documentation of the pretty printer.
* Restructure unit tests into modular chunks.
* Ant support for running unit tests
* Import tests from CLISP and SBCL.
* Unit tests for exception conditions.
* Interactive documentation
 
### How to use cl-format ###

#### Installing using the JAR ####

The easiest way to install cl-format is just to grab the the jar from
github: 
[http://github.com/tomfaulhaber/cl-format/raw/master/release/cl-format.jar](http://github.com/tomfaulhaber/cl-format/raw/master/release/cl-format.jar). This
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
* show_doc - some utilities for showing documentation from various name spaces.

### Differences from the Common Lisp format function ###

The floating point directives that show exponents (~E, ~G) show E for
the exponent character in all cases (unless overridden with an
*exponentchar*).  Clojure does not distinguish between floats and
doubles in its printed representation and neither does cl-format.

The ~A and ~S directives accept the colon prefix, but ignore it since
() and nil are not equivalent in Clojure.

Clojure has 3 different reader syntaxes for characters. The ~@c
directive to cl-format has an argument extension to let you choose:

* ~@c (with no argument) prints "\c" (backslash followed by the
printed representation of the character or \newline, \space, \tab,
\backspace, \return)
* ~'o@c prints "\oDDD" where DDD are the octal digits representing the
character. 
* ~'u@c prints "\uXXXX" prints the hex Unicode representation of the
character.  
