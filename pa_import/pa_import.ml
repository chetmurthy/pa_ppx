(* camlp5r *)
(* pa_import.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_passthru ;
open Ppxutil ;

value predicates = ref [] ;
value lookup_path = ref [] ;

value add_predicates s =
  let l = String.split_on_char ',' s in
  let l = filter (fun s -> s <> "") l in
  List.iter (push predicates) l
;

value add_package s =
  let l = String.split_on_char ',' s in
  let l = filter (fun s -> s <> "") l in
  let pl = Findlib.package_deep_ancestors predicates.val l in
  List.iter (fun p ->
      let d = Findlib.package_directory p in
      push lookup_path d) pl
;

value add_include s =
  push lookup_path (Findlib.resolve_path s)
;

value report () =
  let path = lookup_path.val in
  let path = String.concat ":" path in
  Fmt.(pf stderr "[import_type: packages: %s]\n%!" path)
;

value longid_to_string_list li =
  let rec lirec = fun [
    <:longident< $uid:uid$ >> -> [uid]
  | <:longident< $longid:li$ . $uid:uid$ >> -> (lirec li) @ [uid]
  | <:extended_longident< $longid:_$ ( $longid:_$ ) >> -> failwith "longid_to_string_list: LiApp not allowed here"
  ] in
  lirec li
;

value lookup1 fname d =
  let f = Fpath.add_seg (Fpath.v d) fname in
  let r = Bos.OS.File.exists f in
  if Rresult.R.is_ok r then
    if Rresult.R.get_ok r then
      f
    else failwith "caught"
  else failwith "caught"
;

value reparse_cmi infile =
  let x = Cmi_format.read_cmi infile in
  let l = x.Cmi_format.cmi_sign in
  let txt = Fmt.(str "%a%!" Printtyp.signature l) in
  List.map fst (fst (Pcaml.parse_interf.val (Stream.of_string txt)))
;

value parse_mli infile =
  let txt = infile |> Fpath.v|> Bos.OS.File.read |> Rresult.R.get_ok in
  List.map fst (fst (Pcaml.parse_interf.val (Stream.of_string txt)))
;

value lookup_file suffix fmod =
  let fname = Printf.sprintf "%s.%s" (String.lowercase_ascii fmod) suffix in
  match try_find (fun s -> lookup1 fname s) lookup_path.val with [
    f -> Fmt.(str "%a" Fpath.pp f)
  | exception Failure _ -> failwith (Printf.sprintf "lookup_file: module %s (%s) not found" fmod suffix)
  ]
;

value find1lid lid = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
  try_find (fun td -> if Pcaml.unvala (snd (Pcaml.unvala td.tdNam)) = lid then (td,tdl) else failwith "caught") tdl
| _ -> failwith "caught"
]
;

value find_lid lid sil =
  match try_find (find1lid lid) sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_lid: %s" lid)
  ]
;

value find1mod mname = fun [
  <:sig_item< module $_flag:rf$ $list:l$ >> ->
    try_find (fun (uido, mt, _) ->
      match (Pcaml.unvala uido, mt) with [
        (Some uid, MtSig _ sil) when Pcaml.unvala uid = mname -> Pcaml.unvala sil
      | _ -> failwith "caught"
      ]) l
| <:sig_item< module type $i$ = sig $list:sil$ end $itemattrs:_$ >> when i = mname -> sil
| _ -> failwith "find1mod"
]
;

value find1modty mname = fun [
  <:sig_item< module type $i$ = $mt$ $itemattrs:_$ >> when i = mname -> mt
| _ -> failwith "find1modty"
]
;


value find_mod mname sil =
  match try_find (find1mod mname) sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_mod: %s" mname)
  ]
;

value find_modty mname sil =
  match try_find (find1modty mname) sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_modty: %s" mname)
  ]
;

value find_type modpath lid sil =
  let rec findrec modpath lid sil =
  match modpath with [
    [] -> find_lid lid sil
  | [m :: t] ->
    findrec t lid (find_mod m sil)
  ] in do {
  Fmt.(pf stderr "[find_type: %a]\n%!" (list ~{sep=(const string ".")} string) (modpath@[lid])) ;
  match findrec modpath lid sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_type: %s" (String.concat "." (modpath@[lid])))
  ]
  }
;

value find_module_type modpath sil =
  let rec findrec modpath sil =
  match modpath with [
    [m] ->
    find_modty m sil
  | [m :: t] ->
    findrec t (find_mod m sil)
  ] in do {
  Fmt.(pf stderr "[find_module_type: %a]\n%!" (list ~{sep=(const string ".")} string) modpath) ;
  match findrec modpath sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_type: %s" (String.concat "." modpath))
  ]
  }
;

value lookup_interf fmod =
  try
    reparse_cmi (lookup_file "cmi" fmod)
  with Failure _ ->
    parse_mli (lookup_file "mli" fmod)
;

value lookup_typedecl (fmod, modpath, lid) = do {
  let sil = lookup_interf fmod in
  find_type modpath lid sil
}
;

value lookup_module_type sil = do {
  let (fmod, modpath) = match sil with [ [h::t] -> (h,t) | [] -> assert False ] in
  assert (modpath <> []);
  let sil = lookup_interf fmod in
  find_module_type modpath sil
}
;

value import_typedecl arg t = do {
  report() ;
  match fst (Ctyp.unapplist t)  with [
    <:ctyp< $lid:lid$ >> -> failwith "self-type-lookup not implemented"
  | <:ctyp< $longid:modname$ . $lid:lid$ >> ->
    let sl = longid_to_string_list modname in
    let (fmod, modpath) = match sl with [ [] -> failwith "import_type: internal error" | [h::t] -> (h,t) ] in
    lookup_typedecl (fmod, modpath, lid)
  ]
}
;

value substitute_ctyp renmap t =
  let rec subrec = fun [
    <:ctyp:< ' $id$ >> when List.mem_assoc id renmap ->
      <:ctyp< ' $(List.assoc id renmap)$ >>
  | <:ctyp:< [ $list:l$ ] >> ->
    let l = List.map (fun (loc, cid, tyl, tyo, attrs) ->
        (loc, cid, Pcaml.vala_map (List.map subrec) tyl, option_map subrec tyo, attrs)
      ) l in
      <:ctyp< [ $list:l$ ] >>
  | <:ctyp< $lid:_$ >> | <:ctyp< $longid:_$ . $lid:_$ >> as z -> z
  ] in
  subrec t
;

value rec import_type arg t =
  match t with [
    <:ctyp< $t$ [@ $_attribute:attr$ ] >> ->
      import_type arg t
  | t ->
    let (t, actuals) = Ctyp.unapplist t in
    let (td, tdl) = import_typedecl arg t in
    let formals = Pcaml.unvala td.tdPrm in
    if List.length formals <> List.length actuals then
      failwith "import_type: type-param formal/actual list-length mismatch"
    else let renmap = List.fold_left2 (fun renmap f a ->
        match (Pcaml.unvala (fst f), a) with [
          (None, _) -> renmap
        | (Some fid, <:ctyp< ' $id$ >>) when fid <> id -> [ (fid, id) :: renmap ]
        | _ -> renmap
        ]) [] formals actuals in
    if renmap = [] then td.tdDef
    else substitute_ctyp renmap td.tdDef
  ]
;

value rec import_module_type arg t =
  match t with [
    <:ctyp< $t$ [@ $_attribute:attr$ ] >> ->
      import_module_type arg t
  | <:ctyp< ( module  $longid:li$ . $lid:i$ ) >> ->
      let sl = longid_to_string_list li in
      lookup_module_type (sl@[i])
  | <:ctyp< ( module  $longid:li$ ) >> ->
      let sl = longid_to_string_list li in
      lookup_module_type sl
  ]
;

value registered_ctyp_extension arg = fun [
  <:ctyp:< [% import: $type:t$ ] >> ->
    Some (import_type arg t)
| _ -> assert False
]
;

value registered_module_type_extension arg = fun [
  <:module_type:< [% import: $type:t$ ] >> ->
    Some (import_module_type arg t)
| _ -> assert False
]
;

value install () =
let ef = EF.mk() in
let ef = EF.{ (ef) with
  ctyp = extfun ef.ctyp with [
    <:ctyp:< [% import: $type:_$ ] >> as z ->
      fun arg ->
        registered_ctyp_extension arg z
  ] } in

let ef = EF.{ (ef) with
  module_type = extfun ef.module_type with [
    <:module_type:< [% import: $type:_$ ] >> as z ->
      fun arg ->
        registered_module_type_extension arg z
  ] } in
Pa_passthru.install ("pa_import", ef)
;

Pcaml.add_option "-pa_import-package" (Arg.String add_package)
  "<string> list of packages to search for CMI files.";

Pcaml.add_option "-pa_import-predicates" (Arg.String add_predicates)
  "<string> list of findlib predicates to add when searching CMI files.";

Pcaml.add_option "-pa_import-I" (Arg.String add_include)
  "<string> include-directory to search for CMI files.";

Findlib.init() ;
add_include (Findlib.ocaml_stdlib());
