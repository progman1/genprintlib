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

(*
type ('a, 'b) gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)
*)

open Format
module LocalPrinter :
  sig
    type t
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
  (*
  val install_generic_printer :
          Path.t -> Path.t ->
          (int -> (int -> t -> Outcometree.out_value,
                   t -> Outcometree.out_value) gen_printer) ->
          unit
    val install_generic_printer' :
           Path.t -> Path.t ->
           (formatter -> t -> unit,
            formatter -> t -> unit) gen_printer ->
           unit
*)
    (** [install_generic_printer' function_path constructor_path printer]
        function_path is used to remove the printer. *)

    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> Types.type_expr -> Outcometree.out_value
  end

val unabstract_type:
  Path.t -> Env.t -> (Env.t -> Outcometree.out_value) -> Outcometree.out_value
val debug_on_module: Location.t -> string -> Env.t
