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

value fmt_expression arg param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [
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
  let fmtf = match List.assoc i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.fold: unrecognized param-var in type-decl"
  ] in
  <:expr< $lid:fmtf$ >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "fold" "nobuiltin" ->
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
    
    let body = List.fold_right (fun (v,fmtf) body ->
        <:expr< let acc = $fmtf$ acc $lid:v$ in $body$ >>) vars_fmts <:expr< acc >> in

    (conspat, <:vala< None >>, body)

  | (_, _, _, Some _, _) -> assert False
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
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some li, lid)
    ] in
    let conspat = <:patt< ( # $lilongid:lili$ as a ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< $fmtf$ acc a >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, body) = fmt_record loc arg fields in
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
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let eqfname = fold_fname arg tyname in
  let e = fmt_top arg param_map ty in

  let paramfun_patts = List.map (fun (_,eqf) -> <:patt< $lid:eqf$ >>) param_map in
  [(eqfname, Expr.abstract_over paramfun_patts
      <:expr< fun arg -> $e$ arg >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let foldfname = fold_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< 'fresh -> $pty$ -> 'fresh >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let thety = Ctyp.applist ty paramtys in
  let foldftype = Ctyp.arrows_list loc argfmttys <:ctyp< 'fresh -> $thety$ -> 'fresh >> in
  [(foldfname, foldftype)]
;

value str_item_funs arg td =
  let loc = fst (uv td.tdNam) in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let l = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  List.map (fun (fname, body) ->
      let fty = List.assoc fname types in
      let fty = PM.quantify_over_ctyp param_map fty in
      let attrwarn39 = <:attribute_body< "ocaml.warning" "-39" ; >> in
      let attrwarn39 = <:vala< attrwarn39 >> in
      (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [attrwarn39] >>)) l
;

value sig_items arg td =
  let loc = fst (uv td.tdNam) in
  let param_map = PM.make "fold" loc (uv td.tdPrm) in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_fold0 arg td =
  str_item_funs arg td
;

value loc_of_type_decl td = fst (uv td.tdNam) ;

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
; expr = (fun arg e -> assert False)
; str_item = str_item_gen_fold
; sig_item = sig_item_gen_fold
})
;

