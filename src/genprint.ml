open Typedtree

(* allow for specifying recursive search on a directory *)
let rec expand acc name=
  if Sys.is_directory name then
    let content = Array.map (fun f -> Filename.concat name f) @@ Sys.readdir name in
    name :: (Array.fold_left expand [] content) @ acc
  else
    acc

let flatten (l: string list) : string list =
  let excludes = ref [] in
  let mk_unrec acc h =
    let hl=String.length h in
    if hl>1 && (h.[0]='r' || h.[0]='R') && h.[1]=' ' then
      let dirs = expand [] @@ String.sub h 2 (hl - 2) in
      let _=print_endline @@ "rec "^ (String.sub h 2 (String.length h - 2)) in
      dirs @ acc
    else
    if hl>1 && (h.[0]='x' || h.[0]='X') && h.[1]=' ' then(
      excludes := String.sub h 2 (hl - 2)  :: !excludes;
      acc)
    else
      if hl>0 then h :: acc else acc
  in
  (* a/b/c - a/b *)
  let exclude prefix d =
    String.length prefix > String.length d ||
    String.sub d 0 (String.length prefix) <> prefix

  in

  let expanded = List.fold_left mk_unrec [] l in
  List.fold_left (fun acc x -> List.filter (exclude x) acc) expanded !excludes

(* dune et al, put the build artefacts away from the source *)
let search_dirs= ref(
  "." :: match Sys.getenv "CMTPATH" with
    | s->
      let ss = String.split_on_char ':' s in flatten ss
    | exception Not_found-> [])

(* cmi locations are cmtpath + rec stdlib *)
let _=
  Config.load_path := !search_dirs @  expand [] Config.standard_library @ !Config.load_path

let search file=
  let exception Found of string in
  let exists dir =
    let file=Filename.concat dir file in
    if Sys.file_exists file then
      raise (Found file)
  in
  try
    List.iter exists !search_dirs;
    None
  with
    Found f-> Some f


let genprint = Longident.parse "Genprint.print"
let cache=Hashtbl.create 5

(* genprintval will silently <abstr> what it cannot find so flush out as an error now *)
let iter_type_expr env ty =
  let rec it_path p=
      (* is it local? *)
    if Ident.global (Path.head p) then
      let m= p |> Path.head |> Ident.name in
      try
        (* not finding it now is flagged *)
        ignore @@ Env.find_type p env
      with Not_found ->
        failwith("Genprint: cannot find cmi for module [" ^ m ^"]")

  and iter = {Btype.type_iterators with it_path} in
  iter.it_type_expr iter ty


open TypedtreeIter
module M : IteratorArgument =
struct
  include DefaultIteratorArgument

  let enter_expression =
    function
    (* stop the iteration when a %pr found *)
    | {exp_desc = Texp_apply(
        {exp_desc = Texp_ident (_p, lid, {Types.val_loc=_x; _}); exp_loc=_loc},
        [
          _;                     (* the string *)
          _, Some {exp_desc=Texp_tuple [ (* the value of any type, with extras stuffed in *)
              e;
              {exp_desc=Texp_constant(Const_int count)};
              {exp_desc=Texp_constant(Const_string(file,_))};
           ]}
        ])
      ; exp_loc=_apploc}      when lid.txt = genprint ->
      (* Printf.printf "adding %d/%s to cache\n" count file; *)

      (* OCAML_BINANNOT_WITHENV=1 or can fail ? *)
      (* walk the type_expr looking for anything not in the initial environment, namely
         external references *)
      iter_type_expr e.exp_env e.exp_type;

      Hashtbl.add cache (count, file) (e.exp_type,e.exp_env)

    | _->()
end

module I = MakeIterator(M)

(* store the cmi/crc's for this executable *)
let crc_interfaces = Consistbl.create ()
(*
let interfaces = ref ([] : string list)

let add_import s =
  imported_units := StringSet.add s !imported_units

let store_infos cu =
  let store (name, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> add_import

  in
    printf "\t%s\t%s\n" crc name

  in
  List.iter store cu.cu_imports
*)


let bytecode ic =
  Bytesections.read_toc ic;
  let toc = Bytesections.toc () in
  let toc = List.sort Stdlib.compare toc in
  List.iter
    (fun (section, _) ->
       try
         let len = Bytesections.seek_section ic section in
         if len > 0 then match section with
           | "CRCS" ->
             List.iter (function
                   | _, None->()
                   | name, Some (crc) ->
                     Consistbl.set crc_interfaces name crc ""
               )
               (input_value ic : (string * Digest.t option) list)

           | _->()
       with _ -> ()
    ) toc


(* populate the crc table *)
(* consistency checking by loading the infos of the running exec *)
let _=
  let prog = Sys.executable_name in
  let prog =
    if Filename.is_relative prog then
      Filename.concat(Sys.getcwd()) (Filename.basename prog)
    else
      prog
  in

  let ic = open_in_bin prog in
  let len_magic_number = String.length Config.cmo_magic_number in

  (* assume a bytecode exec for now *)
    let pos_trailer = in_channel_length ic - len_magic_number in
    let _ = seek_in ic pos_trailer in
    let magic_number = really_input_string ic len_magic_number in

  if magic_number = Config.exec_magic_number then begin
    bytecode ic;
    close_in ic;
  end
  else
    (* a native exec does not carry the used-module info present in bytecode. fail or go on?!  *)
    (* failwith "Genprint: unknown excutable format" *)
    ()

(*
let _=
  Consistbl.set crc_interfaces "Test" "" ""
*)

(* match imports of cmt *)
let check_consistency cmt =
  Cmt_format.(try
    List.iter
      (fun (name, crco) ->
         match crco with
            None -> ()
          | Some crc ->
              Consistbl.check crc_interfaces name crc "" (*cmt.cmt_sourcefile*))
      cmt.cmt_imports
  with Consistbl.Inconsistency(_name, _source, _auth) ->
    failwith @@ "Genprint: inconsistency between .cmt for "^ cmt.cmt_modname ^" and this program")




module EvalPath = struct
  type valu = Obj.t
  exception Error
(*
  let eval_path env p = try eval_path env p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
*)
 let eval_address _env = failwith "evalpath: unimplemented"
 let eval_path _env _p = failwith "evalpath: unimplemented"
 let same_value _v1 _v2 = failwith "evalpath: unimplemented"
end

module LocalPrinter = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300
let ppf= ref Format.std_formatter


(* genprintval from the ocaml src is copied verbatim as not possible to have toplevel lib in opt form
without hassle. *)
let outval_of_value env obj ty =
  LocalPrinter.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty

let print_value env obj ppf ty =
  !Oprint.out_value ppf (outval_of_value env obj ty)


(*
put out a string identifier, then the value on next line.
ppx knows the src being processed, runs a count to distinguish applications of pr.
as the target value is 'a, the count/file can piggyback it while keeping the types straight.
*)
let print s v' =
  let ppf = ! ppf in
  
  let open Obj in
  let v'' = repr v' in
  if size v'' <> 3 then
    failwith "Genprint.print can only be invoked through the ppx extension syntax.";

  let v = obj(field v'' 0) in
  let count = obj(field v'' 1) in
  let srcfile = obj(field v'' 2) in
  (* let loc = obj(field v'' 3) in *)


  let print()=
    (* let h = Hastbl.hash key in *)
    let key = (count,srcfile) in
    let ty,env = Hashtbl.find cache key in

    (* the print format is limited and ugly - ideal for dissuading users from actually using this
       for anything other than debugging. *)
    Format.fprintf ppf "%s =>\n" s;
    (* dependency on toploop removed because opt version not available. *)
    (* Toploop.print_value env v ppf ty; *)
    print_value env v ppf ty;
    Format.fprintf ppf "@."
  in

  try
    print()

  with Not_found->
    let cmtfile=Filename.remove_extension srcfile ^ ".cmt" in
    match search cmtfile with
    |Some cmtfile->
      let cmt=Cmt_format.read_cmt cmtfile in
      (* ensure the info is consistent with the running program *)
(*
      let intf = cmt.cmt_interface_digest
      and imports = cmt.cmt_imports in
*)
      check_consistency cmt;

      begin match cmt.cmt_annots with
        | Implementation tstr->
          I.iter_structure tstr;
          print()

        | _-> failwith ("Genprint: Expecting typed-tree from CMT file "^cmtfile)
      end
    | None->
      failwith("Genprint: No .cmt file found corresponding to "^srcfile)


