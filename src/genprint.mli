(* t is abstract: this function cannot be called other than through the ppx extension. *)
val print: string -> 'a -> unit

(* depth/steps initialised from Toploop versions *)
val max_printer_depth : int ref
val max_printer_steps : int ref
(* initialised to std_formatter *)
val ppf : Format.formatter ref

