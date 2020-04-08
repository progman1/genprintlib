(* This file is free software. See file "license" for more details. *)

[@@@warning "-26-27-34-39"]
[@@@warning "-16"]

open Typedtree

(* the partial typechecking in will otherwise repeat compiler warnings so turned off   *)
let _=
  Warnings.parse_options false "-a"

(* DEFUNCT. cmt directories had from ppx.context passed in by compiler *)
(* actually not quite - if user must use (preprocess (pps genprint.ppx ...)) and give up
   auto locating of cmts, then Genprint.cmtpath can be used to be explicit.
*)

(* allow for specifying recursive search on a directory *)
let rec expand acc name=
  if Sys.is_directory name then
    let content = Array.map (fun f -> Filename.concat name f) @@ Sys.readdir name in
    name :: (Array.fold_left expand [] content) @ acc
  else
    acc

let expand (l: string list) : string list =
  let excludes = ref [] in
  let mk_unrec acc h =
    let h = String.trim h in
    let hl=String.length h in
    if hl>1 && (h.[0]='r' || h.[0]='R') && h.[1]=' ' then
      let dirs = expand [] @@ String.sub h 2 (hl - 2) in
      (* let _=print_endline @@ "rec "^ (String.sub h 2 (String.length h - 2)) in *)
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

let search_dirs=ref []
let extra_dirs=ref []
(* cmi locations are cmtpath + rec stdlib *)
let set_loadpath dirs =
  (* search_dirs := dirs @ !search_dirs ; *)
  search_dirs := dirs @ !extra_dirs;
#if OCAML_VERSION < (4,08,0)
  Config.load_path := !search_dirs 
(* @  expand [] Config.standard_library @ !Config.load_path *)
#else
  Load_path.init !search_dirs 
(* @  expand [] Config.standard_library *)
#endif

let get_loadpath () = !search_dirs
(*
#if OCAML_VERSION < (4,08,0)
  !Config.load_path
#else
  Load_path.get_paths()
#endif
*)

(* for user to set additional dirs *)
(* ... just set extras and allow the oft called set-loadpath to incorporate it *)
let cmtpath l= 
  try
    let dirs = String.split_on_char ':' l |> expand in
    extra_dirs:= dirs @ [Config.standard_library];
  with exn ->
    failwith("Genprint: incorrect cmtpath format: "^Printexc.to_string exn)

let envar_set name = try ignore@@Sys.getenv name;true with _->false
(* when stdlib-restricted is true that is to say only ocaml libs are regarded as opaque *)
let stdlib_restricted = ref @@ not@@envar_set "GENPRINT_ALL_LIBS"
let all_libs_opaque b= stdlib_restricted:= not b

let excepted file=
  let libmods_with_submod=["ephemeron"] in
(*
#if OCAML_VERSION >= (4,10,0)
  let libmods_with_submod="sys" :: libmods_with_submod in
#endif
*)
  List.exists (fun m ->
#if OCAML_VERSION < (4,07,0)
      file = Filename.concat Config.standard_library (m ^".cmt"))
#else
      file = Filename.concat Config.standard_library ("stdlib__"^ m ^".cmt"))
#endif
    libmods_with_submod


let find_cmt modname=
(* print_endline@@"FIND CMT"^modname; *)
  let file = 
      Misc.find_in_path_uncap !search_dirs (modname ^ ".cmt")
  in
  file

let is_library file=
  (* the problem with taking one step up from the stdlib to capture other installed libs *)
  (* is if some module defines a submodule with a sig constraint applied inside the src -
     it will not be unabstracted. *)
  let stdlib_root =
    if !stdlib_restricted then Config.standard_library else
    Filename.dirname Config.standard_library in
  (* to indicate whether module should be regarded as a not-to-be-typechecked for abstracted
     sub-modules.  *)
  let regard_as_opaque=try
      String.sub (Filename.dirname file) 0 (String.length stdlib_root) = stdlib_root
    && not@@excepted file
    with Invalid_argument _ ->
      false in
  let inlib=regard_as_opaque in
  inlib



(* save cache(s) to disk. computed signatures are expensive and could encompass all
   modules of a project plus its libraries.*)
let cachefile=".genprint.cache" 

(* the count doubles as a discriminator and time-of-addition  *)
let cache_count=ref 0

type globalsig={
    gs_sig: Types.signature_item;
    gs_cmtfile: string;
    gs_timestamp: float;
    gs_unique_id: int;          (* gives order of addition *)
    gs_loadpath: string list;
  }

(* signatures derived from unabstracted parsetrees *)
let sig_cache = Hashtbl.create 5 
(* recording of print calling sites to enable matching typedtree type info  *)
let pr_cache = Hashtbl.create 5

(* loading of cache necessitates re-processing of out-of-date cmt's *)
let process_cmt_fwd = ref (fun _ -> assert false) (* set before load_cache runs *)

let load_cache()=
  try
    let ch=open_in_bin cachefile in
    let (count, scache,tcache) : (int * _ Hashtbl.t * _) = Marshal.from_channel ch in
    close_in ch;
    cache_count:=count;
    (* needs a correct loadpath before running env-of-only-summary. so not using it! *)
    Hashtbl.iter (fun k (p,ty,env) ->
        Hashtbl.add pr_cache k (p,ty, (*Envaux.env_of_only_summary*) env)) tcache;

    let reduced_ty_cache = Hashtbl.(create 5) in
    (* Hashtbl.iter (fun k (p,ty,env) -> Hashtbl.add reduced_ty_cache k (p,ty, (\*Envaux.env_of_only_summary*\) env)) tcache; *)
    (* run through the cache in order of addition and update out-of-date info *)
    let all = ref [] in
    Hashtbl.iter (fun k d->all := (k,d):: !all) scache;
    (* to avoid re-processing of depended-upon modules, ensure they are loaded and so will be
     found should a process-cmt be invoked, by doing so in the original addition order *)
    let sorted = List.sort (fun (_k,gsig) (_k',gsig') ->
                     compare
                       gsig.gs_unique_id
                       gsig'.gs_unique_id
                   ) !all in

    (* re-process the changed cmts in same order of original processing *)
    List.iter (fun (k, gsig) ->
        if (Unix.stat gsig.gs_cmtfile).st_mtime > gsig.gs_timestamp then 
          (set_loadpath gsig.gs_loadpath;
           (* don't need the result - cache updated with it enough  *)
           ignore@@ !process_cmt_fwd k gsig.gs_cmtfile)
        else
          Hashtbl.add sig_cache k gsig
      ) sorted;

    (* intended for insertion into an object but put aside for now *)
    count, sig_cache, pr_cache
  with
  | Sys_error _ -> 0, Hashtbl.create 5, Hashtbl.create 5 
  | _-> failwith@@ "Genprint: corrupted "^ cachefile ^". Try deleting it."


let record_sig modsig file=
  let uid=try
    let gsig = Hashtbl.find sig_cache file in
    gsig.gs_unique_id
    with Not_found->
      let uid= !cache_count in incr cache_count; uid
  in
  let gsig = {gs_sig=modsig;
              gs_cmtfile=file;
              gs_timestamp=(Unix.stat file).st_mtime;
              gs_unique_id=uid;
              gs_loadpath=get_loadpath();
             }
  in
  Hashtbl.replace sig_cache file gsig

let save_cache()=
  let ch=open_out_bin cachefile in
(*
  let reduced_pr_cache = Hashtbl.(create (length pr_cache)) in
  Hashtbl.iter (fun k (p,ty,env) ->
      Hashtbl.replace reduced_pr_cache k (p,ty, Env.keep_only_summary env)) pr_cache;
*)
  Marshal.to_channel ch (!cache_count, sig_cache, pr_cache) [];
  close_out ch

let add_pr = Hashtbl.replace pr_cache
let find_pr = Hashtbl.find pr_cache
let find_gsig = Hashtbl.find sig_cache

(* abandoned for now. needed assignment to a 'let rec' value.
class ['a,'b,'c,'d] cache (fwd: string->string->Types.signature_item) =
  let (c,scache,pcache)=(process_cmt_fwd:=fwd;load_cache()) in
  object (self)
    constraint 'd = Path.t * Types.type_expr * Env.t
    val mutable cache_count= c
    val sig_cache : ('a,'b) Hashtbl.t = scache
    val pr_cache : ('c,'d) Hashtbl.t = pcache
    method find_sig k =Hashtbl.find sig_cache k
    method add_sig file modsig =
      let uid=try
          let gsig = self#find_sig file in
          gsig.gs_unique_id
        with Not_found->
          let uid= cache_count in cache_count <- 1+cache_count; uid
      in
      let gsig = {gs_sig=modsig;
                  gs_cmtfile=file;
                  gs_timestamp=(Unix.stat file).st_mtime;
                  gs_unique_id=uid;
                  gs_loadpath=get_loadpath();
                 }
      in
      Hashtbl.replace sig_cache file gsig

    method find_pr (k: int * string) =Hashtbl.find pr_cache k
    method add_pr k v = Hashtbl.replace pr_cache k v
  end
*)

(*
let predefs=Predef.[|
  path_int;
  path_char;
  path_bytes;
  path_float;
  path_bool;
  path_unit;
  path_exn;
  path_array;
  path_list;
  path_option;
  path_nativeint;
  path_int32;
  path_int64;
  path_lazy_t;
  path_string;
  path_extension_constructor;
  path_floatarray;
|]


(* genprintval will silently <abstr> what it cannot find so flush out as an error now *)
let iter_type_expr env ty =
  let rec it_path p=
      (* is it local? *)
#if OCAML_VERSION < (4,08,0)
    if Ident.global (Path.head p) then
#else
    (* pre 4.08 predefs were created non-global/non-predef so not checked.
       post 4.08 they are created as predefs which also counts as global.
    *)
    (* if true ||not (Array.exists (Path.same p) predefs) then *)
    (* if Ident.global (Path.head p) && not (Array.exists (Path.same p) predefs) then *)
#endif
      try
        (* not finding it now is flagged *)
        ignore @@ Env.find_type p env
      with Not_found ->
        failwith("Genprint: cannot find type [" ^ (Path.name p) ^"]. Adjust CMTPATH?")

  and iter = {Btype.type_iterators with it_path} in
  iter.it_type_expr iter ty
*)


(* store the cmi/crc's for this executable *)
#if OCAML_VERSION < (4,09,0)
let crc_interfaces = Consistbl.create ()
#else
module Consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_interfaces = Consistbl.create ()
#endif
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
(*
  let prog =
    if Filename.is_relative prog then
      Filename.concat(Sys.getcwd()) (Filename.basename prog)
    else
      prog in
*)
  let ic = try
      open_in_bin prog
    with e->print_endline "error"; raise e
  in
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
    (* a native exec does not carry the import info present in bytecode. fail or go on?!  *)
    (* failwith "Genprint: unknown excutable format" *)
    ()

(* match imports of cmt *)
let check_consistency cmt file=
  Cmt_format.(try
    List.iter
      (fun (name, crco) ->
         match crco with
            None -> ()
          | Some crc ->
              Consistbl.check crc_interfaces name crc "" (*cmt.cmt_sourcefile*))
      cmt.cmt_imports
  with Consistbl.Inconsistency(_name, _source, _auth) ->
    failwith @@ "Genprint: inconsistency between "^ file ^" and this program")

(* intercept calls to particular functions in order to grab the types involved *)
let genprint = Longident.parse "Genprint.print"
let genprint_return = Longident.parse "Genprint.print_with_return"
let genprint_printer = Longident.parse "Genprint.install_printer"
let genprint_remove_printer = Longident.parse "Genprint.remove_printer"

(* typedtrees are iterated over to find occurrences of [%pr] et al, associating type info with 
   them *)
let intercept_expression sub exp=
    match exp with
    (* [%pr ... v] and [%prr ...] v *)
    | {exp_desc = Texp_apply(
        {exp_desc = Texp_ident (p, lid, _)},
        [
          _;                     (* the string *)
          _, Some {exp_desc=Texp_tuple [ (* the value of any type, with extras stuffed in *)
              {exp_desc=Texp_constant(Const_int count)};
              {exp_desc=Texp_constant(Const_string(file,_))};
              _;
           ]};
          (* though the ppx used two apply's it ends up merged  *)
          _,Some e
        ])
      ; exp_loc=_apploc}
      (* or ... *)
    | {exp_desc = Texp_apply(
       {exp_desc = Texp_apply(
        {exp_desc = Texp_ident (p, lid, _ )},
        [
          _;                     (* the string *)
          _, Some {exp_desc=Texp_tuple [ (* the value of any type, with extras stuffed in *)
              {exp_desc=Texp_constant(Const_int count)};
              {exp_desc=Texp_constant(Const_string(file,_))};
              _;
           ]}
        ]);
        exp_loc=_apploc}, [_,Some e])}
         when lid.txt = genprint_return || lid.txt = genprint ->

       let env =Envaux.env_of_only_summary e.exp_env in
       add_pr (count, file) (p, e.exp_type, env)

    | {exp_desc = Texp_apply(
        {exp_desc = Texp_ident (_,lid,_)},
        [
          _, Some ({exp_loc=loc} as fn);
          _, Some {exp_desc=Texp_tuple [ (* the value of any type, with extras stuffed in *)
              {exp_desc=Texp_constant(Const_int count)};
              {exp_desc=Texp_constant(Const_string(file,_))};
              _;
           ]};
        ])}
      when lid.txt = genprint_printer || lid.txt = genprint_remove_printer ->

       (* due to the 'apply' context around the call not possible to prevent this case in ppx *)
       begin match fn with
       |{exp_desc=Texp_ident(fnpath,_,_); exp_type=ty; exp_env } ->
         let exp_env = Envaux.env_of_only_summary exp_env in
         add_pr (count, file) (fnpath, ty, exp_env)
       | _-> 
#if OCAML_VERSION < (4,08,0)
          Location.(report_error
#else
          Location.(print_report
#endif
                      Format.err_formatter
                      (error ~loc "Genprint: must be a printer function name\n"));
                    failwith "aborting..."
             (* exit (-1) *)
       end
#if OCAML_VERSION < (4,09,0)
     | _->()
#else
     | other -> Tast_iterator.default_iterator.expr sub other
#endif

(* by 4.08 typedtreeMap -> tast_mapper
   by 4.09 typedtreeMap -> tast_mapper, typedtreeIter -> tast_iterator
 *)
#if OCAML_VERSION < (4,09,0)
open TypedtreeIter
module M : IteratorArgument =
struct
  include DefaultIteratorArgument
  let enter_expression = intercept_expression ()
end
module I = MakeIterator(M)
#else
module I = struct
  let iter_structure = Tast_iterator.(default_iterator.structure
                         {default_iterator with
                           expr=intercept_expression})
end
#endif

let backtrace f a=
  Printexc.record_backtrace true;
  try
    f a
  with exn->
    print_endline "BACKTRACE.....";
    Printexc.print_backtrace stdout;
    raise exn

(* using the PT mapper. no need for mod-exprs as no functors involved *)
(*
open Ast_mapper
let pt_remap =
  {default_mapper with
    module_expr=fun mapper mb ->
                match mb.mod_desc with
                |Pmod_constraint(me,_)->
                  (* first problem! how to recognise an externally referenced module without
the path! it's just too early... *)
*)


(* the order of visitation of modules will reflect for the most part the dependency
graph. when it comes to reprocessing updated cmts the order of that must respect 
the dependency order else stale typing info will be embedded in recomputed sigs.
 *)


(* abandoned attempt to ascertain whether an intf is actually abstracting any types -
assuming it does creates a bit more work. but hey...
let cmi_abstraction env modname newsig=
  (* under dune the cmi will be another directory to the cmt for opt *)
  let file = Misc.find_in_path_uncap !search_dirs (modname ^ ".cmi") in
  try
    let cmi=Cmi_format.read_cmi file in
    let cmisig=cmi.cmi_sign in
    let check newitem=
      (* include sig-a sig-b ... sig-b is the specification that sig-a has to meet  *)
      (* try *)
      (* let unique_values it acc *)
        ignore@@ Includemod.(signatures env ~mark:Mark_neither cmisig [newitem] )
                   (* witn exn->raise exn *)
    in
    (* if modname="Stdlib_obj" then *)

    List.iter check newsig;
    print_endline@@"SIGMOD no change "^modname;
    false
  with exn->
    print_endline@@"SIGMOD has changed! "^modname;
    Location.report_exception Format.std_formatter exn;
    (* print_endline@@"->>>"^Printexc.to_string exn; *)
    true
*)

(* the cmt's of modules appearing in module-expressions are processed recursively
to anticipate use in functor applications which tends to yield new types,
while regular types are processed on demand by having the genprintval call out when
encountering an abstract type.

modules-for-tc is the recursive collection of modules depended upon, already processed
and for which there now exists an unabstracted signature. 
in this way the current module can be re-typechecked in the presence of those unabstracted
module signatures rather than abstracted cmi-located ones.
 *)
let modules_for_tc = ref []

(* by 4.08 typedtreeMap -> tast_mapper
   by 4.09 typedtreeMap -> tast_mapper, typedtreeIter -> tast_iterator
 *)
#if OCAML_VERSION < (4,08,0)
module type S = module type of TypedtreeMap.MakeMap(TypedtreeMap.DefaultMapArgument)
open TypedtreeMap
#endif
(* module type S = module type of Ttmap.MakeMap(Ttmap.DefaultMapArgument) *)
(* identify earliest structure item changed due to removal of signature or a functor application
   composed of a global module(s) (assumed to be abstracting something relevant),
   and splitting the structure in two.
*)
let rec split (str:structure) =
  (* strip out constraints and process referenced modules' cmts *)
  let sz=List.length str.str_items in
  let count=ref 0 in
  (* note which items are altered *)
  let stritems_slots=Array.make sz false in

  (* the mapper will visit all levels but only want to recurse into idents when they are
     part of a functor application. other usages don't lead to new types?  *)

  let proc_global p=
    if Ident.global (Path.head p) then(
      module_for_tc p;
      (* regard the module as having been abstracted in some way *)
      stritems_slots.(!count) <- true);
  in

#if OCAML_VERSION < (4,08,0)
  let level=ref 0 in

  let unconstrain_mod_enter me=
    match me.mod_desc with
    | Tmod_apply _->
       incr level;
       me
    | _->me
  in
  let unconstrain_mod me=
    match me.mod_desc with
    | Tmod_ident(p,_lidloc) ->
       (* only consider a mident when inside an application and then assume it was 
          abstracted in some way rather than troubling to track exactly how and if relevantly
          abstracted. *)
       if !level>0 then proc_global p;
       me

    | Tmod_apply(fn,farg,c)->
       (* only ident modules referred to in functor applications *)
       decr level;
       me

    | Tmod_constraint(me2,_mt_ty, Tmodtype_explicit _, _mco)->
       (* dropping the abstracting sig *)
       stritems_slots.(!count) <- true;
       me2

(*
    | Tmod_constraint(me2,_mt_ty, Tmodtype_implicit, _mco)->
       print_endline "mod imp constraint"; me
    | Tmod_functor _->
       print_endline "mod functor"; me
    | Tmod_unpack _->
       print_endline "mod unpack"; me
    | Tmod_structure _->
       print_endline "mod struct"; me
*)
(*
    (* implicits added by tc? they can be left in place. doesn't count as a tree change as
       only to reach in to the idents *)
   | Tmod_constraint(me2,mt,Tmodtype_implicit,_)->
       (* ok to drop this auto-gen as untypeast strips it out anyway *)
*)
    | _->me
  in
  (* since the function override is specialised use a module-expr *)
  let (module Map : S) =
    (module MakeMap(
                struct
                  include DefaultMapArgument
                  (* when leaving else the tc will not see unabstracted components *)
                  let enter_module_expr = unconstrain_mod_enter
                  let leave_module_expr = unconstrain_mod
                end))
  in
  let remapped=List.map (fun si ->
                   let si=Map.map_structure_item si in
                   incr count;
                   si)
                 str.str_items in

#else
  (* this mapper incorporates the overrides in the recursion so no double trouble *)
  let rec unconstrain_mod in_app sub me=
    match me.mod_desc with
    | Tmod_ident(p,_lidloc) ->
       if in_app then proc_global p;
       me
    | Tmod_apply(fn,farg,c)->
       (* direct the mapper to only ident modules referred to in functor applications *)
       let sub={sub with Tast_mapper.module_expr=unconstrain_mod true} in
       let fn = unconstrain_mod true sub fn
       and farg = unconstrain_mod true sub farg in
       {me with mod_desc=Tmod_apply(fn,farg,c)}

    (* implicits added by tc? they can be left in place *)
    | Tmod_constraint(me2,_mt_ty, Tmodtype_explicit _, _mco)->
    (* | Tmod_constraint(me2,_mt_ty, _, _mco)-> *)
       (* dropping the abstracting sig and noting the change *)
       stritems_slots.(!count) <- true;
       unconstrain_mod in_app sub me2

    | _ -> Tast_mapper.default.module_expr sub me
  in
  let mapper=Tast_mapper.{default with module_expr=unconstrain_mod false} in
  let remapped=List.map (fun si ->
                   let si=mapper.structure_item mapper si in
                   incr count;
                   si)
                 str.str_items in
#endif
  (* find the earliest stritem modified *)
  let mem x a =
    let open Array in
    let n = length a in
    let rec loop i =
      if i = n then raise Not_found
      else if compare (unsafe_get a i) x = 0 then i
      else loop (succ i) in
    loop 0
  in
  assert(sz>0);
  try
    (* must tc all from 1st changed item so scan results array *)
    let i = mem true stritems_slots in
(* let i=0 in *)
    let unchanged,changed = let n=ref 0 in List.partition (fun _-> incr n; !n<=i) remapped in
    (* the initial env to tc the changed with, is the initial-env of its 1st element *)
    let env=(List.hd changed).str_env in
    (unchanged,changed, env)
  with Not_found->
    (* otherwise no mods - no items to tc *)
    (remapped, [], Env.empty)

and process_cmt modname file=
  let inlib = is_library file in
  let cmt = Cmt_format.read_cmt file in
  check_consistency cmt file;
  let str = match cmt.cmt_annots with
    | Implementation str-> str
    | _ ->
       (* if it didn't compile how can there be an exec? *)
       failwith ("Genprint: "^modname^".cmt file is not complete. Failed compilation?")
  in
  let sign, changed, initial_env =
    (* when the module is of the stdlib/libdir need only obtain the struct sig *)
    if inlib then
      (* assume a library module does not need any processing other than by dodging its .cmi *)
      str.str_type, false, cmt.cmt_initial_env
    else
      (* save the state for calling function *)
      let pre_sigs = !modules_for_tc in
      modules_for_tc:=[];
      (* split the structure according presence of functor applications/sig removal *)
      let unchanged, changed, tc_env = split str in

      (* Printf.printf "SPLIT         %s  %d(%d/%d)\n" modname (List.length str.str_items)(List.length unchanged)(List.length changed); *)
      let changed_str, sign, new_initial_env = 
        if changed<>[] then        (* something to tc *)
          let str={str with str_items=changed}in
          try
            (* env-of often fails for lack of a path to a cmi *)
            let tc_env = Envaux.env_of_only_summary tc_env in
            let pstr = Untypeast.untype_structure str in
            let uniq_mods = List.fold_left (fun acc sg ->
                                if List.memq sg acc then acc else sg::acc)
                              [] !modules_for_tc in
            let senv = Env.add_signature uniq_mods tc_env in

#if OCAML_VERSION < (4,08,0)
             let changed_str, sign, _env = Typemod.type_structure senv pstr Location.none in
#else
             let changed_str, sign, _, _env = Typemod.type_structure senv pstr Location.none in
#endif
             (* the new signature might expose unabstracted types  *)
             let env = Env.add_signature uniq_mods cmt.cmt_initial_env in
             changed_str, sign, env
          with exn ->
            Location.report_exception Format.std_formatter exn;
            (* assert false *)
            failwith ("Genprint: unable to process module "^modname^". Perhaps cmt/load-path is not correct.")
        else
        (* no need for any tc *)
          (* in which case there is nothing to change about the initial env *)
          {str with str_items=[]}
         ,[]
         ,cmt.cmt_initial_env
      in
      modules_for_tc:=pre_sigs;

(* Printf.printf "STATS: %s ==> unch=%d chg=%d ==%d, partstr=%d sig=%d origsig=%d\n" 
 *   modname
 *       (List.length unchanged)
 *       (List.length changed)
 *       (List.length str.str_items)
 *       (List.length changed_str.str_items)
 *       (List.length sign)
 *       (List.length str.str_type)
 * ; *)
      (* recompose the two halves of the structure, unchanged and the re-tc'd *)
      let newstr={changed_str with str_items=unchanged@ changed_str.str_items} in
      (* collection of %pr's now with unabstracted types *)
      I.iter_structure newstr;
      (* just shadow the existing decls *)
      str.str_type @ sign, changed<>[], new_initial_env
    in
    let modid = Ident.create_persistent modname in
    let md_loc = Location.none in
    let modsig=
#if OCAML_VERSION < (4,08,0)
      Types.Sig_module(modid,
                       {md_type = Mty_signature sign;
                        md_attributes = [];
                        md_loc;
                       },
                       Trec_not
      )
#else
      Types.Sig_module(modid, Mp_present,
                       {md_type = Mty_signature sign;
                        md_attributes = [];
                        md_loc;
                       },
                       Trec_not,
                       Exported
                      (* Hidden? *)
      )
#endif
    in
    record_sig modsig file;
    modsig

(* the module in which a %pr appears doesn't need to augment an env with its sig
   as each %pr will pick up a new env directly from regenerated typedtree. *)

(* a visited file's generated signature *)
and find_sig modname=
  (* module names are resolved in the context of the current loadpath so
     in case of name re-use better to use the path as a key. *)
  let cmtfile = find_cmt modname in
  try
    let gsig = find_gsig cmtfile(*modname*) in
    gsig.gs_sig
  with Not_found->
    process_cmt modname cmtfile

and process_local_cmt modname=
  ignore@@
    try
      find_sig modname
    with Not_found->
      failwith("Genprint: No .cmt file found corresponding to "^modname)

and module_for_tc p =
  (* Stdlib.Map.Make - really want Stdlib__map.Make *)
#if OCAML_VERSION < (4,08,0)
  let p=Env.normalize_path None Env.empty p in
#else
  let p=Env.normalize_module_path None Env.empty p in
#endif
  let modid=Path.head p in
  let modname=Ident.name modid in
  try
    let modsig = find_sig modname in
    modules_for_tc := modsig :: !modules_for_tc;
  (* if non-existent just allow to remain abstract which will be intercepted in [unabstract]  *)
  with Not_found-> ()

(* and st = new cache process_cmt *)

(* probably unnecessary but too much in the way for now *)
let _= process_cmt_fwd:=process_cmt
let _=load_cache()


(* side-step the abstract types of a cmi to get at the declarations *)
let unabstract_type p env mkout =
  let modid = Path.head p in
  if not@@Ident.global modid then      (*  *) raise Not_found;
  let modname = Ident.name modid in 
  let modsig = find_sig modname in
  (* references to this module will now not consult the .cmi *)
  let newenv =   Env.add_signature [modsig] env in

  (* is the wanted type still abstract? Bigarray.Genarray.t is external/opaque *)
  begin
    let decl = Env.find_type p newenv in
    match decl with
    | {type_kind = Type_abstract; type_manifest = None} -> raise Not_found (* <abstr> *)
    | _-> ()
  end;

  (* remove the exact type leaving the module path *)
  let p = match p with
#if OCAML_VERSION < (4,08,0)
    | Pdot(p,_,_) -> p
#else
    | Pdot(p,_) -> p
#endif
    | _ -> assert false in

  (* open the just added module to avoid repetitive prefixing  *)
  let newenv =
    (* without_cmis shouldn't be needed as the modid is defined now *)
#if OCAML_VERSION < (4,06,0)
    let mdecl=Env.find_module p newenv in
    match mdecl.md_type with
    | Mty_signature sg ->
       Env.(without_cmis (open_signature Fresh p) sg newenv)
    | _ -> assert false
#else
    match Env.(without_cmis (open_signature Fresh p) newenv) with
    | Some env->env
    | None->assert false 
#endif
    | exception Not_found -> assert false
  in
  let open Outcometree in
  let printer ppf = 
  (* type name not wanted, only the path preceding it *)
    let modname =
      (* Oprint puts out Stdlib__xxxx so this is inconsistent with that ...*)
#if OCAML_VERSION >= (4,07,0)
      Printtyp.rewrite_double_underscore_paths newenv p |> Path.name
#else
      Path.name p
#endif
    in
    let wrap out=
      Format.fprintf ppf "%s." modname;
      !Oprint.out_value ppf out
    in
    (* want M.t value to display as M.(v) when no curlies/parenths/brackets *)
      (* rerun the printing with the augmented env *)
      match mkout newenv with
      | Oval_stuff "<abstr>" as abs -> !Oprint.out_value ppf abs (* no wrapping of this *)
      (* | Oval_constr _ *)
      | Oval_record _
        | Oval_variant _
      (* as it was overcoming abstraction that brought us here, prepend the module path for these
         too *)
      | Oval_stuff _ | Oval_tuple _ | Oval_array _ as l -> wrap l
      | out -> wrap @@ Oval_tuple [out]
  in
  Oval_printer printer

module EvalPath = struct
  type valu = Obj.t
  exception Error
(*
  let eval_path env p = try eval_path env p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
*)
 let eval_address _env = failwith "evalpath: unimplemented"
 (* let eval_path _env _p = failwith "evalpath: unimplemented" *)
 let eval_path _env _p = Obj.repr 0
 (* let same_value _v1 _v2 = failwith "evalpath: unimplemented" *)
 (* let same_value v1 v2 = (v1 == v2) *)
 (* as this is originally for the toplevel not sure if it is relevant here.
    assuming homonyms not possible and extension paths always resolve uniquely *)
 let same_value v1 v2 = true

 let unabstract_type = unabstract_type
end

module LocalPrinter = Genprintval.Make(Obj)(EvalPath)

(* as per the defaults of the toploop *)
let max_printer_depth = ref 100
let max_printer_steps = ref 300
let formatter= ref Format.std_formatter


(* genprintval from the ocaml src is copied verbatim as not possible to have
   toplevel lib in opt form without hassle. *)
let outval_of_value env obj ty =
  LocalPrinter.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty

let print_value env obj ppf ty =
  !Oprint.out_value ppf (outval_of_value env obj ty)

let printing_disabled= envar_set "GENPRINT_NOPRINT"

let unpack inf=
  let open Obj in
  let inf = repr inf in
  if size inf <> 3 then
    failwith "Genprint.print can only be invoked through the ppx extension syntax.";

  let count = obj(field inf 0) in
  let srcfile = obj(field inf 1) in
  let loadpath = obj(field inf 2) in
  set_loadpath loadpath;

(*
print_endline "load path...";
List.iter (fun i-> Printf.printf "LOAD: %s\n" i) loadpath;
print_endline "load path...";
  print_endline @@"run directory: "^Sys.getcwd();
*)

  (count,srcfile)

(*
put out a string identifier, then the value on next line.
ppx knows the src being processed, runs a count to distinguish applications of pr.
as the target value is 'a, the count/file can piggyback it while keeping the types straight.
*)
let print_joint return s inf v =
  if printing_disabled then Obj.(if return then magic v else magic ()) else
  let open Obj in
  let v = magic v in
  let count,srcfile = unpack inf in
  let ppf = ! formatter in
  let print()=
    let key = (count,srcfile) in
    let _p,ty,env = find_pr key in
    (* the print format is limited and ugly - ideal for dissuading users from actually using this
       for anything other than debugging. *)
    Format.fprintf ppf "%s=> " s;
    (* dependency on toploop removed because opt version not available. *)
    (* Toploop.print_value env v ppf ty; *)
    print_value env v ppf ty;
    Format.fprintf ppf "@.";
    Obj.(if return then magic v else magic ())
  in
  try
    (* the first print of the module executed will fault *)
    print()

  with Not_found->
    (* doesn't work without the loadpath setup! so the cache envs are unique to the file's
       loadpaths and cannot be meddled with ie. summary-of, prior *)
    (* init_cache(); *)
    (* loadpath stored as value in each module then transmitted through each print tuple to here
       but only needed once per module *)
    (* set_loadpath loadpath; *)
    let modname =
      Filename.remove_extension srcfile
      |> String.capitalize_ascii 
    in
    (* process should combine consistency check and collecting of %prs,
       for abtract faulting only the sig is wanted*)
    process_local_cmt modname;
    print()

let print             s i v = print_joint false s i v
let print_with_return s i v = print_joint true s i v




type 'a printer_type = Format.formatter -> 'a -> unit

let printer_type env =
#if OCAML_VERSION < (4,09,0)
  Env.lookup_type (Ldot(Lident "Genprint", "printer_type")) env
#else
  fst @@ Env.lookup_type ~loc:Location.none (Ldot(Lident "Genprint", "printer_type")) env
#endif

let match_simple_printer_type env ty printer_type =
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify env
    (Ctype.newconstr printer_type [ty_arg])
#if OCAML_VERSION < (4,08,0)
    (Ctype.instance_def ty);
#else
    (Ctype.instance ty);
#endif
  Ctype.end_def();
  Ctype.generalize ty_arg;
  (ty_arg, None)


let match_printer_type env p =
  let vd= Env.find_value p env in
  let printer_type_new = printer_type env in
#if OCAML_VERSION < (4,08,0)
  Ctype.init_def(Ident.current_time());
#endif
  match_simple_printer_type env vd.val_type printer_type_new


let printer_joint install fn inf =
  if not@@ printing_disabled then
  let open Obj in
  let fn : 'a printer_type = magic fn in
  let count,srcfile = unpack inf in
  let install()=
    let key = (count,srcfile) in
    let p,ty,env = find_pr key in
    (* let env = Envaux.env_of_only_summary env in *)
    let (ty_arg, ty) =
      match_printer_type env p in
    match ty with
    | None ->
       if install then
         LocalPrinter.install_printer p ty_arg fn
       else
         LocalPrinter.remove_printer p
    | _-> assert false
  in
  let modname =
    Filename.remove_extension srcfile
    |> String.capitalize_ascii in
  process_local_cmt modname;
  install()

let install_printer fn inf = printer_joint true fn inf
let remove_printer fn inf = printer_joint false fn inf

let _=
  at_exit save_cache


