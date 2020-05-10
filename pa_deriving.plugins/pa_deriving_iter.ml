(* camlp5r *)
(* pa_deriving_iter.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;

value iter_fname arg tyname =
  if tyname = "t" then "iter"
  else "iter_"^tyname
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< $pty$ -> Stdlib.Unit.t >> ; end) ;

value fmt_expression arg param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [
    <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = iter_fname arg lid in
  <:expr< $lid:fname$ >>

  | <:ctyp:< _ >> -> <:expr< fun _ -> () >>
  | <:ctyp:< unit >> -> <:expr< fun _ -> () >>
  | <:ctyp:< int >> -> <:expr< fun _ -> () >>
  | <:ctyp:< int32 >> -> <:expr< fun _ -> () >>
  | <:ctyp:< int64 >> -> <:expr< fun _ -> () >>
  | <:ctyp:< nativeint >> -> <:expr< fun _ -> () >>
  | <:ctyp:< float >> -> <:expr< fun _ -> () >>
  | <:ctyp:< bool >> -> <:expr< fun _ -> () >>
  | <:ctyp:< char >> -> <:expr< fun _ -> () >>
  | <:ctyp:< string >> -> <:expr< fun _ -> () >>
  | <:ctyp:< bytes >> -> <:expr< fun _ -> () >>

  | <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> List.iter $fmt1$ a >>

  | <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> Array.iter $fmt1$ a >>

  | (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
    let fmt1 = fmtrec ty in
    <:expr< fun a -> $fmt1$ a.val >>

  | <:ctyp:< lazy_t $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [ lazy x ->  $fmt1$ x ] >>

  | <:ctyp:< option $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [
            None -> ()
          | (Some s) -> $fmt1$ a ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun [
          (Result.Ok a) -> $(fmtrec ty1)$ a
        | (Result.Error a) -> $(fmtrec ty2)$ a
      ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.iter: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "iter" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec t

| <:ctyp:< $lid:lid$ >> ->
  let fname = iter_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = iter_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v,_) -> <:patt< $lid:v$ >>) vars_fmts in

    let flditers = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = <:expr< do { $list:flditers$ } >> in

    <:expr< fun ( $list:var1pats$ ) -> $cmpexp$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, _) ->
    let cid = uv cid in
    let (rec1pat, body) = fmt_record loc arg (uv fields) in

    let conspat = <:patt< ($uid:cid$ $rec1pat$) >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var1pats in    
    let flditers = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = <:expr< do { $list:flditers$ } >> in

    (conspat, <:vala< None >>, cmpexp)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>


| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> var1pats in

    let flditers = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = <:expr< do { $list:flditers$ } >> in

    (conspat, <:vala< None >>, cmpexp)

  | PvInh _ ty ->
    let lili = match ty with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some li, lid)
    ] in
    let conspat = <:patt< ( # $lilongid:lili$ as a ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< $fmtf$ a >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, body) = fmt_record loc arg fields in
  <:expr< fun $rec1pat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_iter.fmt_expression"
  ]
  and fmt_record loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "a_%s" fname, fmtrec ty)) fields in

  let v1_pl = List.map (fun (f, v,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v1pat = <:patt< { $list:v1_pl$ } >> in
  let flditers = List.map (fun (v, v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) labels_vars_fmts in
  let cmpexp = <:expr< do { $list:flditers$ } >>
  in
  (v1pat, cmpexp)
 in
  fmtrec ty0
;

value fmt_top arg params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  fmt_expression arg params t2
| t -> fmt_expression arg params t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let ty = td.tdDef in
  let param_map = PM.make "iter" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let eqfname = iter_fname arg tyname in
  let e = fmt_top arg param_map ty in

  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  [(eqfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg -> $e$ arg >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "iter" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let iterfname = iter_fname arg tyname in
  let paramtys = List.map (PM.param_ctyp loc) param_map in
  let argfmttys = List.map (PM.arg_ctyp loc) param_map in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let thety = Ctyp.applist ty paramtys in
  let iterftype = Ctyp.arrows_list loc argfmttys <:ctyp< $thety$ -> Stdlib.Unit.t >> in
  [(iterfname, iterftype)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "iter" loc (uv td.tdPrm) in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  PM.wrap_type_constraints loc param_map funs types
    
;

value sig_items arg td =
  let loc = loc_of_type_decl td in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_iter0 arg td =
  str_item_funs arg td
;

value str_item_gen_iter name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
  let l = List.concat (List.map (str_item_gen_iter0 arg) tdl) in
  <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_iter name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "iter"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"]
; expr_extensions = []
; expr = (fun arg e -> assert False)
; str_item = str_item_gen_iter
; sig_item = sig_item_gen_iter
})
;

