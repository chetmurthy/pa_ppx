(* camlp5r *)
(* pa_deriving_make.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;

value make_fname arg base =
  if base = "t" then "make"
  else Printf.sprintf "make_%s" base
;

value is_main_attribute a = match uv a with [ <:attribute_body< main >> -> True | _ -> False ] ;
value is_split_attribute a = match uv a with [ <:attribute_body< split >> -> True | _ -> False ] ;
value is_default_attribute a = match uv a with [ <:attribute_body< default $exp:_$ ; >> -> True | _ -> False ] ;

value extract_default_attribute a = match uv a with [
  <:attribute_body< default $exp:e$ ; >> -> e
| _ -> failwith "extract_default_attribute"
]
;

value reorder_fields arg ltl =
  let (mainfields, rest) = filter_split (fun (_, _, _, _, al) ->
      List.exists is_main_attribute (uv al)) ltl in
  match mainfields with [
    [] -> (False, ltl)
  | [ _ ; _ :: _ ] -> failwith "pa_deriving.make: reorder_fields: more than one field with main attribute"
  | [f] -> (True, rest @ [f])
  ]
;

value field2req_consfields_funpats loc = (fun (_, fname, _, ty, attrs) ->
    let (default_attrs, attrs) = filter_split is_default_attribute (uv attrs) in
    let ty = ctyp_wrap_attrs ty attrs in
    match (fst (Ctyp.unwrap_attrs ty), default_attrs) with [
      (_, [ _ ; _ :: _ ]) -> failwith "pa_deriving.make: more than one @default attribute"
    | (_, [d]) ->
      let e = extract_default_attribute d in
      (False,
        [(<:patt< $lid:fname$ >>, <:expr< $lid:fname$ >>)],
       [<:patt< ?{ $lid:fname$ = $e$ } >>])
    | (<:ctyp< option $_$ >>, []) ->
      (False,
       [(<:patt< $lid:fname$ >>, <:expr< $lid:fname$ >>)],
       [<:patt< ?{$lid:fname$} >>])
    | (<:ctyp< list $_$ >>, []) ->
      (False,
       [(<:patt< $lid:fname$ >>, <:expr< $lid:fname$ >>)],
       [<:patt< ?{$lid:fname$ = []} >>])
    | (<:ctyp< ( $t1$ * list $t2$ ) >>, [])
      when 1 = count is_split_attribute attrs &&
      fname.[String.length fname - 1] = 's' ->
      let fname_no_s = String.sub fname 0 (String.length fname - 1) in
      (False,
       [(<:patt< $lid:fname$ >>, <:expr< ( $lid:fname_no_s$ , $lid:fname$ ) >>)],
       [<:patt< ~{$lid:fname_no_s$} >>;
         <:patt< ?{$lid:fname$ = []} >>])
    | (_, []) when 1 = count is_main_attribute attrs ->
      (True,
       [(<:patt< $lid:fname$ >>, <:expr< $lid:fname$ >>)],
       [<:patt< $lid:fname$ >>])

    | (_, []) ->
      (True,
       [(<:patt< $lid:fname$ >>, <:expr< $lid:fname$ >>)],
       [<:patt< ~{$lid:fname$} >>])
    ])
;

value fmt_expression arg : ctyp -> expr = fun [
  <:ctyp:< { $list:fields$ } >> ->
  let (has_main, fields) = reorder_fields arg fields in
  
  let req_consfields_funpats = List.map (field2req_consfields_funpats loc) fields in
  let all_required = List.for_all Std.fst3 req_consfields_funpats in

  let funpats = List.concat (List.map Std.third3 req_consfields_funpats) in

  let efields = List.concat (List.map Std.snd3 req_consfields_funpats) in

  let body = <:expr< { $list:efields$ } >> in
  let body = if has_main || all_required then body else <:expr< fun () -> $body$ >> in
  Expr.abstract_over funpats body

| _ -> failwith "Pa_deriving.make: not a record type"
]
;

value fmt_top arg params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  fmt_expression arg t1
| t -> fmt_expression arg t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let makefname = make_fname arg tyname in
  let e = fmt_top arg td.tdPrm ty in

  [(makefname, e)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "make" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let makefname = make_fname arg tyname in
  let fields = match ty with [
    <:ctyp< { $list:ltl$ } >> -> ltl | _ -> failwith "pa_deriving_make: not called on a record type"
  ] in
  let (has_main, fields) = reorder_fields arg fields in
  let req_paramtys = List.map (fun [
      (loc, name, _, (<:ctyp< option $t$ >>), _) -> (False, [TyOlb loc <:vala< name >> t])
    | (loc, name, _, (<:ctyp< list $_$ >> as ty), _) -> (False, [TyOlb loc  <:vala< name >> ty])
    | (loc, name, _, ty, attrs)
      when 1 = count is_default_attribute (uv attrs) -> (False, [TyOlb loc <:vala< name >> ty])
    | (loc, name, _, <:ctyp< ( $t1$ * list $t2$ ) >>, attrs)
      when 1 = count is_split_attribute (uv attrs) &&
      name.[String.length name - 1] = 's' ->
      let name_no_s = String.sub name 0 (String.length name - 1) in
      (False, [TyLab loc <:vala< name_no_s >> t1; TyOlb loc <:vala< name >> <:ctyp< list $t2$ >>])
    | (loc, name, _, ty, attrs)
      when 1 < count is_default_attribute (uv attrs) ->
      failwith (Printf.sprintf "Pa_deriving.make: more than one @default attribute on field %s" name)
    | (loc, name, _, ty, attrs) when 1 = count is_main_attribute (uv attrs) -> (True, [ty])
    | (loc, name, _, ty, attrs) -> (True, [TyLab loc <:vala< name >> ty])
    ]) fields in
    let paramtys = List.concat (List.map snd req_paramtys) in
    let all_required = List.for_all fst req_paramtys in

  let thety = Ctyp.applist <:ctyp< $lid:tyname$ >>
     (List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map) in

  let thety = if has_main || all_required then thety else <:ctyp< unit -> $thety$ >> in
  let makeftype = Ctyp.arrows_list loc paramtys thety in
  [(makefname, makeftype)]
;


value str_item_funs arg td =
  let loc = fst (uv td.tdNam) in
  let param_map = PM.make "make" loc (uv td.tdPrm) in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  wrap_type_constraints loc param_map funs types
;

value sig_items arg td =
  let loc = fst (uv td.tdNam) in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_make0 arg td =
  str_item_funs arg td
;

value loc_of_type_decl td = fst (uv td.tdNam) ;

value str_item_gen_make name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_make0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_make name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "make"
; alternates = []
; options = ["with_path"; "optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ; ("with_path", <:expr< True >>) ]
; alg_attributes = ["opaque"; "printer"; "polyprinter"; "nobuiltin"]
; expr_extensions = []
; expr = (fun arg e -> assert False)
; str_item = str_item_gen_make
; sig_item = sig_item_gen_make
})
;


