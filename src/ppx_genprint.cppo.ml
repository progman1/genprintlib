open Ppxlib
open Caml

(*
extract from compiler call, add to tuple argument to 'print', use to find the cmt file.
        lid "include_dirs", make_list make_string !Clflags.include_dirs;
        lid "load_path",    make_list make_string (Load_path.get_paths ());

the cmt for current file with be stored alongside the object.
the load-path should be byte/native for dune.
difference with include-dirs? load-path includes 'include-dirs under 4.08

*)

let envar_set name = try ignore@@Sys.getenv name;true with _->false
let remove_extensions = envar_set "GENPRINT_REMOVE"

let count = ref 0
let cwd = Sys.getcwd()
let fullpath f=
  if Filename.is_relative f then
    Filename.concat cwd f
  else
    f

let getwords l =
  List.fold_left (fun msg (_,word) ->
      match word.pexp_desc with
      | Pexp_ident id ->
         let word'= Longident.name id.txt in
         (word' ^" "^ msg)
      | Pexp_construct({txt=Lident lid} ,_)->
         (lid ^" "^ msg)
      (* | Pexp_constant _-> *)
      | _ ->
         Location.raise_errorf ~loc:word.pexp_loc "this should be a word")
    "" l

let set_loadpath locopt =
  let loc = match locopt with
    | None-> Location.none
    | Some loc->loc
  in
  (* should be setup from a ppx.context *)
#if OCAML_VERSION < (4,08,0)
  let lp = !Ocaml_common.Config.load_path in
#else
  let lp = Ocaml_common.Load_path.get_paths() in
#endif
  let lp = List.map fullpath lp in
  let expl = [%expr [%e Ast_builder.Default.(
                       (elist ~loc (List.map (estring ~loc) lp))
             )]]
  in
  [[%stri let __loadpath = [%e expl]]], []

(* put this value into every compiled unit once *)
let () =
  Driver.register_transformation
    ~enclose_impl:set_loadpath
    "genprint-loadpath" ~extensions:[]

(* cache mode can be detected by examining data as loaded
(* used internally *)
val ppx_mode: bool ref

let set_ppxmode locopt =
  let loc = match locopt with
    | None-> Location.none
    | Some loc->loc
  in
  [[%stri let _ = Genprint.ppx_mode:=true ]], []

(* put this value into every compiled unit once *)
let () =
  Driver.register_transformation
    ~enclose_impl:set_ppxmode
    "genprint-ppxmode" ~extensions:[]
*)


let expand ~loc ~path (e : expression) =
  incr count;
  (*free-form text before the value including capitalisation *)
  let words, e =
    match e.pexp_desc with
    |Pexp_apply(e,el)->
      begin
        match List.rev el with
        | [] -> assert false          (* would not be an apply without something *)
        | (_,v)::tl ->                (* the last arg is the expression to eval *)
           let msg = getwords @@ tl@[Nolabel,e] in
          msg,v
      end
    | _-> "",e
  in
  if remove_extensions then [%expr let _=[%e e] in ()] else
  [%expr [%e Ast_builder.Default.(
   pexp_apply ~loc (
    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.print") ))
      [Nolabel,estring ~loc words;
       Nolabel,pexp_tuple ~loc [ 
                   (eint ~loc !count);
                   (estring ~loc @@ Filename.basename path);
                   (pexp_ident ~loc (Located.mk ~loc (Longident.parse "__loadpath") ));
                 ]
      ])
    [Nolabel,e]
  )]]


let genprint =
  Extension.declare
    "pr"
    Extension.Context.expression
    Ast_pattern.(pstr ((pstr_eval __ nil) ^:: nil))
    expand


let () =
  Driver.register_transformation
    "genprint" ~extensions:[ genprint ]

let expand ~loc ~path (payload : payload) = 
  incr count;
  (* let open Ast_builder.Default in *)
  let words = match payload with
    |PStr [] -> ""
    |PStr [{pstr_desc=Pstr_eval(e,_)}]->
      begin match e.pexp_desc with
      |Pexp_apply(e,el)-> getwords @@ (List.rev el) @[Nolabel,e]
      | _-> getwords [Nolabel,e]
      end
    | _ -> Location.raise_errorf ~loc "improper syntax: empty or non-keyword word and Words"
  in
  if remove_extensions then [%expr fun x->x] else

  [%expr [%e Ast_builder.Default.(
    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.print_with_return") ))
      [Nolabel,estring ~loc words;
       Nolabel,pexp_tuple ~loc [
                   (eint ~loc !count);
                   (estring ~loc @@ Filename.basename path);
                   (pexp_ident ~loc (Located.mk ~loc (Longident.parse "__loadpath") ));
                 ]
      ]
  )]]

let genprint =
  Extension.declare
    "prr"
    Extension.Context.expression
    (* Ast_pattern.(pstr ((pstr_eval __ nil) ^:: nil)) *)
    Ast_pattern.( (( __ ) ))
    expand


let () =
  Driver.register_transformation
    "genprint-with-return" ~extensions:[ genprint ]

(* custom printer installation *)
let expand ~loc ~path (fn : expression) = 
  incr count;
  if remove_extensions then [%expr let _=[%e fn] in ()] else

  [%expr [%e Ast_builder.Default.(
    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.install_printer") ))
      [Nolabel,fn;
       Nolabel,pexp_tuple ~loc [
                   (eint ~loc !count);
                   (estring ~loc @@ Filename.basename path);
                   (pexp_ident ~loc (Located.mk ~loc (Longident.parse "__loadpath") ));
                 ]
      ]
  )]]

let genprint =
  Extension.declare
    "install_printer"
    Extension.Context.expression
    Ast_pattern.(pstr ((pstr_eval __ nil) ^:: nil))
    expand


let () =
  Driver.register_transformation
    "genprint-install-printer" ~extensions:[ genprint ]

let remove_printer ~loc ~path (fn : expression) = 
  incr count;
  if remove_extensions then [%expr let _=[%e fn] in ()] else

  [%expr [%e Ast_builder.Default.(
    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.remove_printer") ))
      [Nolabel,fn;
       Nolabel,pexp_tuple ~loc [
                   (eint ~loc !count);
                   (estring ~loc @@ Filename.basename path);
                   (pexp_ident ~loc (Located.mk ~loc (Longident.parse "__loadpath") ));
                 ]
      ]
  )]]

let genprint =
  Extension.declare
    "remove_printer"
    Extension.Context.expression
    Ast_pattern.(pstr ((pstr_eval __ nil) ^:: nil))
    remove_printer


let () =
  Driver.register_transformation
    "genprint-remove-printer" ~extensions:[ genprint ]


(*
let expand_ff ~loc ~path (lid : longident) (el : (arg_label * expression) list) =
...

let genprint_freeform =
    Extension.declare
      "prs"
      Extension.Context.expression
      (* pattern appearing in [%pr <sss> v] is the application of the ident to the value *)
      Ast_pattern.(pstr ((pstr_eval (pexp_apply (pexp_ident __) ( __)  ) nil ) ^:: nil))
      expand_ff
*)
