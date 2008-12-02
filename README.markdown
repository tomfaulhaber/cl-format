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

### Current Status ###

cl-format is under active development. Currently it supports only simple, linear directives (no
brackets) and does not yet support the various floting point formats. Next on the list is the
bracket constructions.

### How to use ###

### Examples ###

TBD

### Differences from Common Lisp format ###

