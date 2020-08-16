(* camlp5r *)
(* pa_deriving_fold.ml,v *)
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

value fold_fname arg tyname =
  if tyname = "t" then "fold"
  else "fold_"^tyname
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc ty = assert False ; end) ;

value fmt_expression arg ?{coercion} param_map ty0 =
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [
    <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = fold_fname arg lid in
  <:expr< $lid:fname$ >>

  | <:ctyp:< _ >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< unit >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< int >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< int32 >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< int64 >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< nativeint >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< float >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< bool >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< char >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< string >> -> <:expr< fun acc _ -> acc >>
  | <:ctyp:< bytes >> -> <:expr< fun acc _ -> acc >>

  | <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun acc a -> List.fold_left $fmt1$ acc a >>

  | <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun acc a -> Array.fold_left $fmt1$ acc a >>

  | (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
    let fmt1 = fmtrec ty in
    <:expr< fun acc a -> $fmt1$ acc a.val >>

  | <:ctyp:< lazy_t $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun acc -> fun [ lazy x ->  $fmt1$ acc x ] >>

  | <:ctyp:< option $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun acc -> fun [
            None -> acc
          | (Some a) -> $fmt1$ acc a ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun acc -> fun [
          (Result.Ok a) -> $(fmtrec ty1)$ acc a
        | (Result.Error a) -> $(fmtrec ty2)$ acc a
      ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.fold: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when Some id = DC.allowed_attribute (DC.get arg) "fold" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec t

| <:ctyp:< $lid:lid$ >> ->
  let fname = fold_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = fold_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v,_) -> <:patt< $lid:v$ >>) vars_fmts in

    let body = List.fold_right (fun (v,fmtf) body ->
        <:expr< let acc = $fmtf$ acc $lid:v$ in $body$ >>) vars_fmts <:expr< acc >> in
    

    <:expr< fun acc ( $list:var1pats$ ) -> $body$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    <:constructor:< $uid:cid$ of { $list:fields$ } $algattrs:_$ >> ->
    let (rec1pat, body) = fmt_record loc arg fields in

    let conspat = <:patt< ($uid:cid$ $rec1pat$) >> in
    (conspat, <:vala< None >>, body)

  | <:constructor:< $uid:cid$ of $list:tyl$ $algattrs:_$ >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var1pats in    
    
    let body = List.fold_right (fun (v,fmtf) body ->
        <:expr< let acc = $fmtf$ acc $lid:v$ in $body$ >>) vars_fmts <:expr< acc >> in

    (conspat, <:vala< None >>, body)

  | (_, _, _, <:vala< Some _ >>, _) -> assert False
  ]) l in
  <:expr< fun acc -> fun [ $list:branches$ ] >>


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

    let body = List.fold_right (fun (v,fmtf) body ->
        <:expr< let acc = $fmtf$ acc $lid:v$ in $body$ >>) vars_fmts <:expr< acc >> in

    (conspat, <:vala< None >>, body)

  | PvInh _ ty ->
    let lili = match ty with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some <:vala< li >>, lid)
    ] in
    let conspat = <:patt< ( # $lilongid:lili$ as a ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< $fmtf$ acc a >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, body) = fmt_record loc arg fields in
  let rec1pat = match coercion with [ None -> rec1pat | Some ty -> <:patt< ( $rec1pat$ : $ty$ ) >> ] in
  <:expr< fun acc -> fun $rec1pat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_fold.fmt_expression"
  ]
  and fmt_record loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "a_%s" fname, fmtrec ty)) fields in

  let v1_pl = List.map (fun (f, v,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v1pat = <:patt< { $list:v1_pl$ } >> in
  let body = List.fold_right (fun (_,v,fmtf) body ->
      <:expr< let acc = $fmtf$ acc $lid:v$ in $body$ >>) labels_vars_fmts <:expr< acc >> in
  (v1pat, body)
 in fmtrec ?{coercion=coercion} ty0
;

value fmt_top arg ~{coercion} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  fmt_expression arg ~{coercion=coercion} params t2
| t -> fmt_expression arg ~{coercion=coercion} params t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let tk = td.tdDef in
  let tyname = uv tyname in
  let ty =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    (Ctyp.applist ty paramtys) in
  let coercion =
    monomorphize_ctyp ty in
  let eqfname = fold_fname arg tyname in
  let e = fmt_top arg ~{coercion=coercion} param_map tk in

  let paramfun_patts = List.map (PM.arg_patt ~{naked=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let argexp =
    if uv td.tdPrv && is_type_abbreviation tk then
      <:expr< ( arg : $monomorphize_ctyp ty$ :> $monomorphize_ctyp tk$ ) >>
    else <:expr< arg >> in
  [(eqfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun acc arg -> $e$ acc $argexp$ >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let foldfname = fold_fname arg tyname in
  let paramtys = List.map (PM.param_ctyp loc) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< 'fresh -> $pty$ -> 'fresh >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let thety = Ctyp.applist ty paramtys in
  let foldftype = Ctyp.arrows_list loc argfmttys <:ctyp< 'fresh -> $thety$ -> 'fresh >> in
  [(foldfname, foldftype)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
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

value str_item_gen_fold0 arg td =
  str_item_funs arg td
;

value str_item_gen_fold name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
  let l = List.concat (List.map (str_item_gen_fold0 arg) tdl) in
  <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_fold name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "fold"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_fold
; sig_item = sig_item_gen_fold
})
;

