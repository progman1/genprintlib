open Ppxlib

let count = ref 0

let expand ~loc ~path (msg: string) (e : expression) =
  incr count;
(* Printf.printf "GOT: %s -> %s/%d\n" path msg !count; *)
  [%expr [%e Ast_builder.Default.(

    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.print") ))
      [Nolabel,estring ~loc msg;
       Nolabel,pexp_tuple ~loc [ e; (eint ~loc !count); (estring ~loc @@ Filename.basename path) ]
      ]
  )]]


let genprint =
  Extension.declare
    "prs"
    Extension.Context.expression
    (* pattern appearing in [%pr "..." v] is the application of the string to the value *)
    Ast_pattern.(pstr ((pstr_eval (pexp_apply (estring __)
                                     (
                                      no_label __ ^:: nil)   ) nil ) ^:: nil))
    expand


let () =
  Driver.register_transformation "genprint" ~extensions:[ genprint ]


let expand_ff ~loc ~path (lid : longident) (el : (arg_label * expression) list) =
  incr count;
  match List.rev el with
  | [] -> assert false
  | (_,e) :: tl ->
    let first_word = Longident.name lid in
    let msg = List.fold_left (fun msg (_,word) ->
        match word.pexp_desc with
        | Pexp_ident id ->
          let word'= Longident.name id.txt in
          (" "^ word' ^msg)
        | _ ->
          Location.raise_errorf ~loc:word.pexp_loc "this should be a word")
        "" tl in

  [%expr [%e Ast_builder.Default.(
    pexp_apply ~loc
      (pexp_ident ~loc (Located.mk ~loc (Longident.parse "Genprint.print") ))
      [Nolabel,estring ~loc (first_word ^ msg);
       Nolabel,pexp_tuple ~loc [ e; (eint ~loc !count); (estring ~loc @@ Filename.basename path) ]
      ]
  )]]

let genprint_freeform =
    Extension.declare
      "pr"
      Extension.Context.expression
      (* pattern appearing in [%pr "..." v] is the application of the string to the value *)
      Ast_pattern.(pstr ((pstr_eval (pexp_apply (pexp_ident __) ( __)  ) nil ) ^:: nil))
      expand_ff




let () =
  Driver.register_transformation "genprint-freeform" ~extensions:[ genprint_freeform ]

