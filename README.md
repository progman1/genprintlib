# Genprint

A one function library and ppx extension to provide general value printing
anywhere in a program, as opposed to the ```ocaml``` toplevel behaviour of only printing
evaluated expression results.

Used like so:

``` ocaml
[%pr "text..." v]
```
where ```v``` is an arbitrary value.

The type of the value is retrieved from the containing source file's .cmt file
which can be had via the compiler option [-bin-annot] or the recommended way of
setting an environment variable permanently via .profile etc:

```
export OCAMLPARAM="_,-bin-annot=1"
```
This will ensure that all compilations generate annotation files, which are needed anyway for
__Merlin__ to function fully.

If using __Dune__ or another build manager that places build artefacts other than alongside the source
files then another environment variable needs setting:

```
export CMTPATH=<colon delimited list of directories to search>
```

With __Dune__ a possible invocation might be:

```
CMDPATH=_build/default/test/.test.eobjs/byte dune run test
```

where a [test] directory contains some source code.


The library is [genprint] and the ppx extension is [genprintppx].

# Limitations
Genprint can be used in the ocaml toplevel as long as the value arguments are from modules that have 
previously been compiled and for which a .cmt file exists.

Genprint uses the compiler toplevel internals to do the actual printing and so will
display <poly> for values than do not have an instantiated type (or for parts thereof).
So avoid embedding this printer into a polymorphic context.

The printing will fail (segmentation fault likely) where the assigned type in the .cmt does not correspond to 
the actual value given as argument to [%pr].
This may come about if the wrong .cmt file is retrieved due to searching in an incorrect directory
that happens to have an identically named file but which is unrelated otherwise.



