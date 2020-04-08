(* these functions cannot be called other than through the ppx extension. *)
val print: string -> 't -> 'a -> unit
val print_with_return: string -> 't -> 'a -> 'a

val cmtpath: string -> unit
(* whether to disallow recursive examination of non-ocaml libraries' cmt files.
   default: false=disallow but results in slower printing  *)
val all_libs_opaque: bool -> unit

(* depth/steps initialised as per Toploop versions *)
val max_printer_depth : int ref
val max_printer_steps : int ref
(* initialised to std_formatter *)
val formatter : Format.formatter ref

type 'a printer_type = Format.formatter -> 'a -> unit

val install_printer: 'a printer_type -> 't -> unit
val remove_printer: 'a printer_type -> 't -> unit

