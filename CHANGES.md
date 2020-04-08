Genprint v0.3
----------------

- drawbacks of the prior version dealt with:
  * didn't work at all on 4.08
  * needed an environment variable set to locate .cmt files when working with Dune
  * more often than not data of interest was printed as <abstr> due to signatures.

  location of .cmt files is now handled automatically while abstracted data structures are made 
  visible, which applies to modules both statically defined and in expression form.
  the range of compiler versions tested: 4.04.2 -> 4.10.0

- [%printer <name>] added to install a custom printer as per the REPL

Genprint v0.1/v0.2
----------------------

- intitial release with near instantaneous bug fix!
