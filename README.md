# Genprint

A one function library and PPX extension to provide general value printing
from anywhere within a program, as opposed to the ```ocaml``` toplevel behaviour of printing only
evaluated expression results.

Used like so:

``` ocaml
[%prs "text..." v]
```
or
``` ocaml
[%pr some intro text followed by an expression (v,v) ]
```

where ```v``` is an arbitrary value. It forms a unit-valued expression.

The type of the value is retrieved from the containing source file's .cmt file
which can be had via the compiler option [-bin-annot] or the recommended way of
setting an environment variable permanently via one's .profile etc:

```
export OCAMLPARAM="_,-bin-annot=1"
```
This will ensure that all compilations generate annotation files, which are needed anyway for
__Merlin__ to function fully.

If using __Dune__ or another build manager that places build artefacts other than alongside the source
files then this environment variable needs setting:

```
export CMTPATH=<colon delimited list of directories to search>
```

With __Dune__ a possible invocation might be:

```
CMTPATH=_build/default/test/.test.eobjs/byte dune exec ./test/test.exe
```
as per [test/dune](test/dune).

Directories can be marked for recursive search:
```
CMTPATH=r\ _build dune  exec ./test/test.exe
```
Note the escape of the space. Also note that quoting prevents expansion of ~.

Using recursion can lead to identically named modules from different libraries/contexts being
picked up, so use the exclusion form to prevent such collisions:
```
CMTPATH=r\ _build dune:x\ _build/default/bad/.bad.eobjs/byte exec ./test/test.exe exec ./test/test.exe
```

The variable name CMTPATH is a misnomer - it is also used like [ocamlc -I ...] for .cmi files which
are also needed for printing. 
For instance [%pr the stdlib [%here] ] would need to find ```Lexing.cmi```.
The OCaml standard library location is already included (recursively) so this prints, but the 
rest of the directories under one's switch library directory are not.

For building, the library is [genprint] and the PPX extension is [genprint.ppx], for both byte and optimising compilation:
```
ocamlc -ppx '~/.opam/default/lib/genprint/ppx/ppx.exe --as-ppx' -I +../genprint genprint.cma <src>
```
and with ocamlfind:

```
ocamlfind ocamlc -package genprint -package genprint.ppx -linkpkg  <name>.ml -o <name>
```

# Limitations
This Genprint library cannot be used in the ocaml toplevel except in as much as loading objects already
compiled with embedded printing statements and for which a .cmt file exists.

Genprint uses the compiler internals to do the actual printing and so will
display ```<poly>``` for values than do not have an instantiated type (or for parts thereof).
So avoid embedding this printer into a polymorphic context ie. do not try creating a printing function
around this.

With the optimising compiler, the printing will fail (segmentation fault likely) where the assigned type in the .cmt does not correspond to 
the actual value given as argument to [%pr].
This may come about if the wrong .cmt file is retrieved due to searching in an incorrect directory
that happens to have an identically named file but which is unrelated otherwise.


# Lament For The -plugin Option
Having implemented Genprint as a compiler variant, then as this library, I became aware of
the ```-plugin``` option and re-implemented to suit. It meant value types (to guide the printing) 
no longer came from .cmt files (therefore no CMTPATH setup), and no need of a PPX extension,
just

``` ocaml
open Genprint
   ...
   prs "some label..." x;
   ...
```
and which could be used within the toplevel too.

But then I discovered ```-plugin``` was scheduled to be removed from the compiler as of 4.09!
What a pity.
I hope another means of compiler extension will come in the future that doesn't involve 
creating a new compiler binary (which is anti-compositionality!)

