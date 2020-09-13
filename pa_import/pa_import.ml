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
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value debug = Pa_passthru.debug ;

value mli_only = ref False ;
value redeclare = ref False ;
value predicates = ref [] ;
value lookup_path = ref [] ;

value add_predicates s =
  let l = String.split_on_char ',' s in
  let l = Std.filter (fun s -> s <> "") l in
  List.iter (Std.push predicates) l
;

value add_package s =
  let l = String.split_on_char ',' s in
  let l = Std.filter (fun s -> s <> "") l in
  let pl = Findlib.package_deep_ancestors predicates.val l in
  List.iter (fun p ->
      let d = Findlib.package_directory p in
      Std.push lookup_path d) pl
;

value add_include s =
  Std.push lookup_path (Findlib.resolve_path s)
;

value report () =
  let path = lookup_path.val in
  let path = String.concat ":" path in
  Fmt.(pf stderr "[import_type: packages: %s]\n%!" path)
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
  let fname = Printf.sprintf "%s.%s" (String.uncapitalize_ascii fmod) suffix in
  match try_find (fun s -> lookup1 fname s) lookup_path.val with [
    f -> Fmt.(str "%a" Fpath.pp f)
  | exception Failure _ -> failwith (Printf.sprintf "lookup_file: module %s (%s) not found" fmod suffix)
  ]
;

value find1lid lid = fun [
  <:sig_item:< type $flag:nrfl$ $list:tdl$ >> ->
  try_find (fun td -> if uv (snd (uv td.tdNam)) = lid then (td,(nrfl, tdl)) else failwith "caught") tdl
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
      match (uv uido, mt) with [
        (Some uid, MtSig _ sil) when uv uid = mname -> uv sil
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
  if debug.val then Fmt.(pf stderr "[find_type: %a]\n%!" (list ~{sep=(const string ".")} string) (modpath@[lid])) else () ;
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
  if debug.val then Fmt.(pf stderr "[find_module_type: %a]\n%!" (list ~{sep=(const string ".")} string) modpath) else () ;
  match findrec modpath sil with [
    x -> x
  | exception Failure _ -> failwith (Printf.sprintf "find_type: %s" (String.concat "." modpath))
  ]
  }
;

value logged_parse f fname =
  let st = Unix.gettimeofday () in
  let rv = f fname in
  let et = Unix.gettimeofday () in do {
    Fmt.(pf stderr "[parse %s in %f secs]\n%!" fname (et -. st)) ;
    rv
  }
;
value do_lookup_interf fmod =
  try
    if not mli_only.val then
      logged_parse reparse_cmi (lookup_file "cmi" fmod)
    else failwith "caught"
  with Failure _ ->
    logged_parse parse_mli (lookup_file "mli" fmod)
;

value interface_cache = ref [] ;
value lookup_interf fmod =
  match List.assoc fmod interface_cache.val with [
    x -> x
  | exception Not_found ->
    let rv = do_lookup_interf fmod in do {
      Std.push interface_cache (fmod,rv) ;
      rv
    }
  ]
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
  if debug.val then report() else () ;
  match fst (Ctyp.unapplist t)  with [
    <:ctyp< $lid:lid$ >> -> failwith "self-type-lookup not implemented"
  | <:ctyp< $longid:modname$ . $lid:lid$ >> ->
    let sl = Longid.to_string_list modname in
    let (fmod, modpath) = match sl with [ [] -> failwith "import_type: internal error" | [h::t] -> (h,t) ] in
    lookup_typedecl (fmod, modpath, lid)
  ]
}
;

module RM = struct
  value (=) = Reloc.eq_ctyp ;
  value rec assoc x = fun [
    [] -> raise Not_found
  | [(a,b)::l] -> if  a = x then b else assoc x l ];

value rec mem_assoc x = fun [
    [] -> False
  | [(a, _) :: l] ->  a = x || mem_assoc x l ];

end
;

value substitute_ctyp renmap t =
  let rec subrec = fun [
    <:ctyp:< ( $list:l$ ) >> -> <:ctyp< ( $list:List.map subrec l$ ) >>
  | <:ctyp:< { $list:ldl$ } >> ->
    let sub_label_decl (loc, na,b,ty,al) =
      (loc,na,b,subrec ty, al) in
    <:ctyp< { $list:List.map sub_label_decl ldl$ } >>
  | <:ctyp:< [ $list:l$ ] >> ->
    let l = List.map (fun (loc, cid, tyl, tyo, attrs) ->
        (loc, cid, Pcaml.vala_map (List.map subrec) tyl, vala_map (option_map subrec) tyo, attrs)
      ) l in
      <:ctyp< [ $list:l$ ] >>
  | <:ctyp:< $_$ $_$ >> as z ->
    subst1 (Ctyp.unapplist z)
  | t when RM.mem_assoc t renmap ->
      RM.assoc t renmap
  | z -> subst1 (z,[])
  ]
  and subst1 (t,args) =
    let args = List.map subrec args in
    if not (RM.mem_assoc t renmap) then Ctyp.applist t args
    else match RM.assoc t renmap with [
      <:ctyp:< $t$ [@ "polyprinter" $e$ ; ] >> ->
        <:ctyp< $Ctyp.applist t args$ [@ "polyprinter" $e$ ; ] >>
    | t -> Ctyp.applist t args
    ]
  in subrec t
;

value string_list_of_expr e =
  let rec srec = fun [
    <:expr< $lid:i$ >> -> [i]
  | <:expr< $uid:i$ >> -> [i]
  | <:expr< $e1$ . $e2$ >> -> (srec e1) @ (srec e2)
  ]
  in srec e
;

value expr_to_ctyp0 loc e = do {
  let l = string_list_of_expr e in
  let (last,l) = sep_last l in
  assert (last = String.uncapitalize_ascii last) ;
  match l with [
    [] -> <:ctyp< $lid:last$ >>
  | [h::t] ->
    let li = List.fold_left (fun li i -> <:longident< $longid:li$ . $uid:i$ >>)
        <:longident< $uid:h$ >> t in
    <:ctyp< $longid:li$ . $lid:last$ >>
  ]}
;

value expr_to_ctyp loc e =
  match e with [
    <:expr:< $e$ [@ $attribute:a$ ] >> ->
    let ct = expr_to_ctyp0 loc e in
    <:ctyp:< $ct$ [@ $attribute:a$ ] >>
  | e -> expr_to_ctyp0 loc e
  ]
;

value assignment_to_subst = fun [
  <:expr:< $e1$ . val := $e2$ >> ->
    let t1 = expr_to_ctyp loc e1 in
    let t2 = expr_to_ctyp loc e2 in
    if Reloc.eq_ctyp t1 t2 then [] else [(t1, t2)]
]
;

value extend_renmap attr renmap =
  let e = match uv attr with [
    <:attribute_body<"with" $exp:e$ ; >> -> e
  | _ -> failwith "import: unrecognized attribute"
  ] in
  let l = match e with [
    <:expr< $_$ := $_$ >> -> assignment_to_subst e
  | <:expr< do { $list:l$ } >> ->
    List.concat (List.map assignment_to_subst l)
  ] in
  l @ renmap
;

value extract_with_attributes attrs =
  (List.filter (fun a -> "with" = (attr_id a)) attrs,
   List.filter (fun a -> "with" <> (attr_id a)) attrs)
;

type unpacked_t =
  {
    full_t : ctyp
  ; attrs : list attribute
  ; bare_t : ctyp
  ; unapp_t : ctyp
  ; args : list ctyp
  ; li : longid
  ; lid : string
  ; sl : list string
  ; loc : Ploc.t
  }
;

value unpack_imported_type full_t =
  let (bare_t,attrs) = Ctyp.unwrap_attrs full_t in
  let (unapp_t, args) = Ctyp.unapplist bare_t in
  let (li, lid) = match unapp_t with [
    <:ctyp< $longid:li$ . $lid:lid$ >> -> (li, lid)
  | _ -> failwith "unpack_imported_type"
  ] in
  let sl = Longid.to_string_list li in
  { full_t = full_t ; attrs = attrs ; bare_t = bare_t ;
    unapp_t = unapp_t ; args = args ; li = li ;
    lid = lid ; sl = sl ; loc = loc_of_ctyp full_t }
;

value import_type arg (newtname,new_formals) t =
  let unp = unpack_imported_type t in
  let (with_attrs, rest_attrs) = extract_with_attributes unp.attrs in
  let unp = { (unp) with attrs = rest_attrs } in
  let renmap = List.fold_right extend_renmap with_attrs [] in
  let loc = unp.loc in
  let actuals = unp.args in
  let (td, tdl) = import_typedecl arg unp.unapp_t in
  let formals = uv td.tdPrm in
    if List.length formals <> List.length actuals then
      failwith "import_type: type-param formal/actual list-length mismatch"
    else let renmap = List.fold_left2 (fun renmap f a ->
        match (uv (fst f), a) with [
          (None, _) -> renmap
        | (Some fid, <:ctyp< ' $id$ >>) when fid <> id ->
          let fid = <:ctyp< ' $fid$ >> in
          [ (fid, a) :: renmap ]
        | _ -> renmap
        ]) renmap formals actuals in
    let oldtname = uv (snd (uv td.tdNam)) in
    let newtname = uv (snd newtname)in
    let renmap = if oldtname = newtname then
        renmap
      else [ (<:ctyp< $lid:oldtname$ >>, <:ctyp< $lid:newtname$ >>) :: renmap ] in
    let tk = match td.tdDef with [
      <:ctyp< $_$ == $t$ >> -> t
    | t -> t
    ] in
    let ct = if renmap = [] then tk
    else Ctyp.wrap_attrs (substitute_ctyp renmap tk) unp.attrs in
    if is_generative_type ct && not redeclare.val then
      <:ctyp< $unp.bare_t$ == $ct$ >>
    else ct
;

value import_typedecl_group arg t item_attrs =
  let unp = unpack_imported_type t in
  let (with_attrs, rest_attrs) = extract_with_attributes unp.attrs in
  let unp = { (unp) with attrs = rest_attrs } in
  let renmap = List.fold_right extend_renmap with_attrs [] in
  let loc = unp.loc in
  let (rd, (nrfl, tdl)) = import_typedecl arg unp.unapp_t in
  let tdl = List.map (fun td ->
      let imported_tycon =
        let tyna = uv (snd (uv td.tdNam)) in
        let baset = <:ctyp< $longid:unp.li$ . $lid:tyna$ >> in
        let type_var2arg (so, _) =
          match uv so with [
            Some s -> <:ctyp< ' $s$ >>
          | None ->
            failwith "import_typedecl_group: cannot import typedecl group where some params are unnamed"
          ] in
        let args = List.map type_var2arg (uv td.tdPrm) in
        Ctyp.applist baset args in
      let ct = if renmap = [] then td.tdDef
        else Ctyp.wrap_attrs (substitute_ctyp renmap td.tdDef) unp.attrs in
      let ct = if is_generative_type ct && not redeclare.val then
          <:ctyp< $imported_tycon$ == $ct$ >>
        else ct in
      { (td) with tdDef = ct }
    ) tdl in
  let (last, tdl) = sep_last tdl in
  let last =
    let last_attrs = uv last.tdAttributes in
    let new_attrs = last_attrs@item_attrs in
    { (last) with tdAttributes = <:vala< new_attrs >> } in
  let tdl = tdl @ [last] in
  (nrfl, tdl)
;

value rec import_module_type arg t =
  match t with [
    <:ctyp< $t$ [@ $attribute:attr$ ] >> ->
      import_module_type arg t
  | <:ctyp< ( module  $longid:li$ . $lid:i$ ) >> ->
      let sl = Longid.to_string_list li in
      lookup_module_type (sl@[i])
  | <:ctyp< ( module  $longid:li$ ) >> ->
      let sl = Longid.to_string_list li in
      lookup_module_type sl
  ]
;

value registered_str_item_extension arg = fun [
  <:str_item:< type $flag:nrfl$ $list:tdl$ >> ->
    let tdl = List.map (fun td ->
        match td.tdDef with [
          <:ctyp< [% import: $type:t$ ] >> ->
          let tname = uv td.tdNam in
          let l = uv td.tdPrm in
          let ct = import_type arg (tname,l) t in
          { (td) with tdDef = ct }
        | _ -> td
        ]
      ) tdl in
    <:str_item< type $flag:nrfl$ $list:tdl$ >>

  | <:str_item:< [%% import: $type:t$ ] $itemattrs:item_attrs$ >> ->
    let (nrfl, tdl) = import_typedecl_group arg t item_attrs in
    <:str_item< type $flag:nrfl$ $list:tdl$ >>
| _ -> assert False
]
;

value registered_sig_item_extension arg = fun [
  <:sig_item:< type $flag:nrfl$ $list:tdl$ >> ->
    let tdl = List.map (fun td ->
        match td.tdDef with [
          <:ctyp< [% import: $type:t$ ] >> ->
          let tname = uv td.tdNam in
          let l = uv td.tdPrm in
          let ct = import_type arg (tname,l) t in
          { (td) with tdDef = ct }
        | _ -> td
        ]
      ) tdl in
    <:sig_item< type $flag:nrfl$ $list:tdl$ >>

  | <:sig_item:< [%% import: $type:t$ ] $itemattrs:item_attrs$ >> ->
    let (nrfl, tdl) = import_typedecl_group arg t item_attrs in
    <:sig_item< type $flag:nrfl$ $list:tdl$ >>
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
  str_item = extfun ef.str_item with [
    <:str_item:< type $flag:_$ $list:_$ >> as z ->
      fun arg _ ->
        Some (registered_str_item_extension arg z)

  | <:str_item< [%% import: $type:_$ ] $itemattrs:_$ >> as z -> 
      fun arg _ ->
        Some (registered_str_item_extension arg z)

  ] } in

let ef = EF.{ (ef) with
  sig_item = extfun ef.sig_item with [
    <:sig_item:< type $flag:_$ $list:_$ >> as z ->
      fun arg _ ->
        Some (registered_sig_item_extension arg z)

  | <:sig_item< [%% import: $type:_$ ] $itemattrs:_$ >> as z -> 
      fun arg _ ->
        Some (registered_sig_item_extension arg z)

  ] } in

let ef = EF.{ (ef) with
  module_type = extfun ef.module_type with [
    <:module_type:< [% import: $type:_$ ] >> as z ->
      fun arg _ ->
        registered_module_type_extension arg z
  ] } in
Pa_passthru.(install { name = "pa_import" ; ef = ef ; pass = None ; before = [] ; after = [] })
;

Pcaml.add_option "-pa_import-package" (Arg.String add_package)
  "<string> list of packages to search for CMI files.";

Pcaml.add_option "-pa_import-predicates" (Arg.String add_predicates)
  "<string> list of findlib predicates to add when searching CMI files.";

Pcaml.add_option "-pa_import-I" (Arg.String add_include)
  "<string> include-directory to search for CMI files.";

Pcaml.add_option "-pa_import-mli-only" (Arg.Set mli_only)
  "<string> use only MLI (not CMI) files.";

Pcaml.add_option "-pa_import-redeclare" (Arg.Set redeclare)
  "<string> redeclare types (do not re-export) -- useful for using types from other Ocaml versions.";

(* calls lazy_init() so we're sure of being inited *)
add_include (Findlib.ocaml_stdlib());

install();
