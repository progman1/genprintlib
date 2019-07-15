open Typedtree

(*
let max_printer_depth = ref !Toploop.max_printer_depth
let max_printer_steps = ref !Toploop.max_printer_steps
let ppf= ref Format.std_formatter
*)

(* the print format is limited and ugly - ideal for dissuading users from actually using this
for anything other than debugging. *)

(* dune et al, put the build artefacts away from the source *)
let search_dirs= ref(
  "." :: match Sys.getenv "CMTPATH" with
    | s->
      let ss = String.split_on_char ':' s in ss
    | exception Not_found-> [])



let add_search_dir dir=
  search_dirs := dir :: !search_dirs

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

open TypedtreeIter
module M : IteratorArgument =
struct
  include DefaultIteratorArgument

  let enter_expression =
    function
    | {exp_desc = Texp_apply(
        {exp_desc = Texp_ident (p, lid, {Types.val_loc; _}); exp_loc},
        [
          _;                     (* the string *)
          _, Some {exp_desc=Texp_tuple [ (* the value of any type, with extras stuffed in *)
              e;
              {exp_desc=Texp_constant(Const_int count)};
              {exp_desc=Texp_constant(Const_string(file,_))};
           ]}
        ])
      ; exp_loc=apploc}      when lid.txt = genprint ->
      (* Printf.printf "adding %d/%s to cache\n" count file; *)
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
  let toc = List.sort Pervasives.compare toc in
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
  with Consistbl.Inconsistency(name, source, auth) ->
    failwith @@ "Genprint: inconsistency between .cmt for "^ cmt.cmt_modname ^" and this program")




module EvalPath = struct
  type valu = Obj.t
  exception Error
(*
  let eval_path env p = try eval_path env p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
*)
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
