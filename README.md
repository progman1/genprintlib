![travis-ci] (https://travis-ci.org/progman1/genprintlib.svg?branch=master)

# Genprint

A one function library and PPX extension to provide general value printing
from anywhere within a program, as opposed to the ```ocaml``` toplevel/REPL behaviour of
printing only evaluated expression results.

Used like so:

``` ocaml
[%pr v];
```
or:

``` ocaml
[%pr arbitrary text with_ Uppercase but no keywords followed by the expression (123,456)];
```
It is a unit value expression therefore terminated by ';'

Another form is available when the printing instruction stands aside from the value:

``` ocaml
[%prr] v
```
or:

``` ocaml
[%prr some descriptive text] (123,456)
```
The result is the value given.

Printing is as per the OCaml REPL and printing depth/maximum steps, can be set along with
the formatter.

``` ocaml
Genprint.max_printer_depth := 100;
Genprint.max_printer_steps := 300;
Genprint.formatter := Format.std_formatter;
```
Two examples:

``` ocaml
let h = Hashtbl.create 1 in
Hashtbl.add h 'X' "the data";
[%pr hash table (888,h,888)];
```
results in:

``` ocaml
hash table => (888,
               Stdlib.Hashtbl.{size = 1;
                               data =
                                [|Empty; Empty; Empty; Empty; Empty;
                                  Cons
                                   {Stdlib__hashtbl.key = 'X';
                                    data = "the data"; next = Empty};
                                   Empty; Empty; Empty; Empty; Empty; Empty;
                                   Empty; Empty; Empty; Empty|];
                                seed = 0; initial_size = 16},
                               888)
```
This printer will look behind abstracting interfaces created by functor application:

``` ocaml
module SM = Map.Make(struct type t = string let compare = compare end)
let s=SM.add "singleton" 'Z' SM.empty in
[%pr stringmap s];
```
for output:

``` ocaml
stringmap => SM.(Node
                  {SM.l = Empty; v = "singleton"; d = 'Z';
                   r = Empty; h = 1})
```


The type of the value is retrieved from the containing source file's .cmt file
which can be had via the compiler option [-bin-annot] or the recommended way of
setting an environment variable permanently in one's .profile etc:

```
export OCAMLPARAM="_,-bin-annot=1"
```
This will ensure that all compilations generate annotation files, which are needed anyway for
__Merlin__ to function fully, and __Dune__ has this flag set by default.

__Genprint__ assumes the standard library defines no sub-modules (excepting Ephemerons).
If there were, while being 
constrained by a signature (in the ml source rather than by mli) then __Genprint__ would not
remove this abstraction and any value described by such a module would be printed as

``` ocaml
<abstr>
```
By default other installed libraries (if OCaml is at [lib/ocaml] then other libraries are assumed
under [lib]) are treated as if they may have such sub-modules. 
This may not always be appropriate as it imposes a performance cost.
If this is annoying the environment variable [GENPRINT_ALL_LIBS=1] can be used to prevent the 
search, which is appropriate if the values one is interested in are not displayed as ```<abstr>```
as a result.
Local project code is always scanned for sub-modules.

# Customised Printing?

There is the syntax form [%install_printer <name>] equivalent to the toplevel/REPL [#install_printer]
that allows to handle those data types that remain abstract, such as [Bigarray.Genarray.t]
which as a C level value has no OCaml representation.

``` ocaml
  let open Bigarray in
  let open Array1 in
  let a1=create Float32 C_layout 1 in
  set a1 0 99.9;
  [%pr bigarray is abstract a1];
  let a1print ppf (a: _ Array1.t) = Format.fprintf ppf "got it...%f" (get a 0) in
  (* %printer only accepts an identifier, not a closure *)
  [%install_printer a1print];
  [%pr bigarray unabstracted a1];
```
Creating a custom printer is what this library seeks to avoid generally. But where it is necessary,
as the foregone example demonstrates, or indeed where there is an existing printing function, it is rather convenient to deploy it through the [%printer] mechanism, especially when values it is concerned with are embedded within other structures, as otherwise one would
have to extend one's printing function or break apart the structure to get to the embedded part
of interest. 

Installation's counterpart:

``` ocaml
  [%remove_printer a1print]
```

# Compiling

For building, the library is [genprint] and the PPX extension is [genprint.ppx], for both byte and optimising compilation:
```
ocamlc -ppx '~/.opam/default/lib/genprint/ppx/ppx.exe --as-ppx' -I +../genprint genprint.cma <src>
```
and with ocamlfind:

```
ocamlfind ocamlc -package genprint -package genprint.ppx -linkpkg  <name>.ml -o <name>
```
and with Dune:

``` ocaml
 ...
 (libraries genprint ... )
 (preprocess (staged_pps genprint.ppx))
 ...
```
where __staged_pps__ is used rather than __pps__ in order for the .cmt file locating
to work.

To compile with print statements left in-place while removing them from the compiled code
set the environment variable [GENPRINT_REMOVE=1].
The resulting parsetree still has no-op artefacts but these are removed
by __ocamlopt__ or the flambda variant.
This would be a nice convenience if it were not for the fact that the resultant binary
is still linked with the compiler-libs library, on which there should be no dependency.
I do not know why that is at present.

To disable printing at runtime with print statements compiled in,
set the variable [GENPRINT_NOPRINT=1].

# Ocamldebug

A personal obstacle to using __ocamldebug__ has been the frustrating appearance of
```<abstr>``` for many a value I wanted to look at, especially at the
thrashing-around-without-a-clue stage of debugging when I'd rather not be coding 
printing functions and installing them under the debugger only to abandon them when it 
becomes apparent a particular value is of no interest after all.
This repo contains two dunified versions of the debugger extracted from compiler sources,
modified to use __Genprint__ as the default printer. The modifications are minor
and easily transferred to other versions.
An example invocation:
``` ocaml
dune exec debugger/4.10.0/main.bc -- <path to sample cmo> -I <includes>
```
or drop into your Dune project as required.
By default __Genprint__ will issue a warning for modules unfound. Values involving types
from such will display as ```<abstr>```. Adjust the include-arguments to correct - unlike
with the emdedded printing such paths are not automatically taken care of.
To disable the warning set the environment variable [GENPRINT_IGNORE_MISSING=1]

# Limitations
This __Genprint__ library cannot be used in the ocaml toplevel except in as much as loading objects already
compiled with embedded printing statements and for which a .cmt file exists.

__Genprint__ uses the compiler internals to do the actual printing and so will
display ```<poly>``` for values that do not have an instantiated type (or for parts thereof).
So avoid embedding this printer into a polymorphic context ie. do not create a printing function
around this without ensuring a concrete type is being inferred by the typechecker or otherwise 
providing an explicit constraint.

Printing can sometimes be slow due to the amount of processing needed to reveal concrete types
built-up from layers of modules. This is minimised after the first compilation due to caching
of results. The file [.genprint.cache] is used for this. It can be deleted at any point.
It should be deleted whenever setting/unsetting the [GENPRINT_ALL_LIBS] environment variable
or changing the value via [Genprint.all_libs_opaque]. 
[GENPRINT_ALL_LIBS=1] or [Genprint.all_libs_opaque true] has the effect of preventing the
recursive analysis of library modules outside of the OCaml library directory.
This only pertains to disovering the presence of functor generated modules and by not searching for
such, printing is faster. On the other hand the introduction of caching renders this less
important.

The abstraction of data types removed by this printer relates only to that introduced at the
module implementation level (.mli interface files) and explicit signatures written into
.ml sources as used in module expressions. It does not deal at all with class types, another 
means of abstraction.
Indeed the REPL printer upon which __Genprint__ is based outputs only
```<obj>``` for any object.

I think, in principle, something better could be achieved.

# Feedback

This is experimental software! If you encounter a problem or can suggest an improvement/addition
please raise an issue.
