(* camlp5r *)
(* pa_deriving_eq.ml,v *)
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

value eq_fname arg tyname =
  if tyname = "t" then "equal"
  else "equal_"^tyname
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< $pty$ -> $pty$ -> Stdlib.Bool.t >> ; end) ;

value fmt_expression arg ?{coercion} param_map ty0 =
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [
    <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = eq_fname arg lid in
  <:expr< $lid:fname$ >>

  | <:ctyp:< _ >> -> <:expr< fun a b -> True >>
  | <:ctyp:< unit >> -> <:expr< fun a b -> True >>
  | <:ctyp:< int >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< int32 >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< int64 >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< nativeint >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< float >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< bool >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< char >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< string >> -> <:expr< fun a b -> a=b >>
  | <:ctyp:< bytes >> -> <:expr< fun a b -> a=b >>

  | <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< let rec loop x y =
        match (x, y) with [
          ([], []) -> True
        | ([a::x], [b::y]) ->
            ($fmt1$ a b) && (loop x y)
        | _ -> False ] in
      fun x y -> loop x y >>

  | <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun x ->
        fun y ->
          let rec loop i =
            (i = (Array.length x)) ||
              (($fmt1$ (x.(i)) (y.(i))) &&
                 (loop (i + 1))) in
          ((Array.length x) = (Array.length y)) && (loop 0) >>

  | (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
    let fmt1 = fmtrec ty in
    <:expr< fun a b -> $fmt1$ a.val b.val >>

  | <:ctyp:< lazy_t $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [ lazy x -> fun [ lazy y  -> $fmt1$ x y ] ] >>

  | <:ctyp:< option $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun x ->
        fun y ->
          match (x, y) with [
            (None, None) -> True
          | (Some a, Some b) -> $fmt1$ a b
          | _ -> False ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun a b -> match (a,b) with [
          (Result.Ok a, Result.Ok b) -> $(fmtrec ty1)$ a b
        | (Result.Error a, Result.Error b) -> $(fmtrec ty2)$ a b
        | _ -> False
      ] [@ocaml.warning "-4";] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.eq: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

  | <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "eq" "equal" -> e

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "eq" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec t

| <:ctyp:< $lid:lid$ >> ->
  let fname = eq_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = eq_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, Printf.sprintf "b_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let var2pats = List.map (fun (_, v, _) -> <:patt< $lid:v$ >>) vars_fmts in

    let fldcmps = List.map (fun (v1, v2, fmtf) -> <:expr< $fmtf$ $lid:v1$ $lid:v2$ >>) vars_fmts in
    let cmpexp = match fldcmps with [
      [h::t] -> List.fold_left (fun e1 e2 -> <:expr< $e1$ && $e2$ >>) h t
    | [] -> assert False ] in

    <:expr< fun ( $list:var1pats$ ) ( $list:var2pats$ ) -> $cmpexp$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, _) ->
    let cid = uv cid in
    let (rec1pat, rec2pat, body) = fmt_record loc arg (uv fields) in

    let conspat = <:patt< ($uid:cid$ $rec1pat$, $uid:cid$ $rec2pat$) >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, Printf.sprintf "b_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let cons1pat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var1pats in
    let var2pats = List.map (fun (_, v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let cons2pat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var2pats in
    let conspat = <:patt< ($cons1pat$, $cons2pat$) >> in
    
    let fldcmps = List.map (fun (v1, v2, fmtf) -> <:expr< $fmtf$ $lid:v1$ $lid:v2$ >>) vars_fmts in
    let cmpexp = match fldcmps with [
      [h::t] -> List.fold_left (fun e1 e2 -> <:expr< $e1$ && $e2$ >>) h t
    | [] -> <:expr< True >> ] in

    (conspat, <:vala< None >>, cmpexp)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  let branches = branches @ [ (<:patt< _ >>, <:vala< None >>, <:expr< False >>) ] in
  <:expr< fun a b -> match (a,b) with [ $list:branches$ ] [@ocaml.warning "-4";][@ocaml.warning "-11";] >>


| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, Printf.sprintf "b_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let cons1pat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> var1pats in
    let var2pats = List.map (fun (_, v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let cons2pat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> var2pats in
    let conspat = <:patt< ($cons1pat$, $cons2pat$) >> in

    let fldcmps = List.map (fun (v1, v2, fmtf) -> <:expr< $fmtf$ $lid:v1$ $lid:v2$ >>) vars_fmts in
    let cmpexp = match fldcmps with [
      [h::t] -> List.fold_left (fun e1 e2 -> <:expr< $e1$ && $e2$ >>) h t
    | [] -> <:expr< True >> ] in

    (conspat, <:vala< None >>, cmpexp)

  | PvInh _ ty ->
    let lili = match ty with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some li, lid)
    ] in
    let conspat = <:patt< (( # $lilongid:lili$ as a ), ( # $lilongid:lili$ as b )) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< $fmtf$ a b >>)
  ]) l in
  let branches = branches @ [ (<:patt< _ >>, <:vala< None >>, <:expr< False >>) ] in
  <:expr< fun a b -> match (a,b) with [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, rec2pat, body) = fmt_record loc arg fields in
  let rec1pat = match coercion with [ None -> rec1pat | Some ty -> <:patt< ( $rec1pat$ : $ty$ ) >> ] in
  let rec2pat = match coercion with [ None -> rec2pat | Some ty -> <:patt< ( $rec2pat$ : $ty$ ) >> ] in
  <:expr< fun $rec1pat$ -> fun $rec2pat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_eq.fmt_expression"
  ]
  and fmt_record loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "a_%s" fname, Printf.sprintf "b_%s" fname, fmtrec ty)) fields in

  let v1_pl = List.map (fun (f, v, _,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v1pat = <:patt< { $list:v1_pl$ } >> in
  let v2_pl = List.map (fun (f, _, v,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v2pat = <:patt< { $list:v2_pl$ } >> in
  let fldcmps = List.map (fun (v, v1, v2, fmtf) -> <:expr< $fmtf$ $lid:v1$ $lid:v2$ >>) labels_vars_fmts in
  let cmpexp = match fldcmps with [
    [h::t] -> List.fold_left (fun e1 e2 -> <:expr< $e1$ && $e2$ >>) h t
  | [] -> assert False ]
  in
  (v1pat, v2pat, cmpexp)
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
  let ty = td.tdDef in
  let param_map = PM.make "eq" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let coercion =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    monomorphize_ctyp (Ctyp.applist ty paramtys) in
  let eqfname = eq_fname arg tyname in
  let e = fmt_top arg ~{coercion=coercion} param_map ty in

  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  [(eqfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg -> $e$ arg >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "eq" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let eqfname = eq_fname arg tyname in
  let paramtys = List.map (PM.param_ctyp loc) param_map in
  let argfmttys = List.map (PM.arg_ctyp loc) param_map in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let thety = Ctyp.applist ty paramtys in
  let eqftype = Ctyp.arrows_list loc argfmttys <:ctyp< $thety$ -> $thety$ -> Stdlib.Bool.t >> in
  [(eqfname, eqftype)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "eq" loc (uv td.tdPrm) in
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

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "eq" loc (uv td.tdPrm) in
    let sil = sig_items arg td in
    let (eqfname, eqftype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" eqfname in
    let field_type = PM.quantify_over_ctyp param_map eqftype in
    [ <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:eqfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:eqfname$ ;
    end >> :: sil ]
| _ -> assert False
]
;

value extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "show" loc (uv td.tdPrm) in
    let (eqfname, eqftype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" eqfname in

    let field_type = PM.quantify_over_ctyp param_map eqftype in
    let fexp = <:expr< fun _ _ -> False >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:eqfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< value $lid:eqfname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (eq_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "eq" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = fmt_expression arg param_map ty in
    let branches = match e with [
      <:expr< fun a b -> match (a,b) with [ $list:branches$ ] [@ $attribute:_$ ] [@ $attribute:_$ ] >> -> branches
    | _ -> assert False
    ] in
    (* remove the catch-branch *)
    let (_, branches) = sep_last branches in 
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< (a,b) >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps @[<:expr< a >> ;  <:expr< b >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun a b -> match (a,b) with [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

| _ -> assert False
]
;

value str_item_gen_eq0 arg td =
  str_item_funs arg td
;

value str_item_gen_eq name arg = fun [
  <:str_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
  let l = List.concat (List.map (str_item_gen_eq0 arg) tdl) in
  <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_eq name arg = fun [
  <:sig_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_sig_items arg z in
    <:sig_item< declare $list:l$ end >>

| <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    <:sig_item< declare $list:[]$ end >>

| <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

value expr_eq arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "eq" || id = "derive.eq" ->
    let loc = loc_of_ctyp ty in
    let coercion = monomorphize_ctyp ty in
    let e = fmt_top arg ~{coercion=coercion} [] ty in
    <:expr< fun a b ->  $e$ a b >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "eq"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["equal"; "nobuiltin"]
; expr_extensions = ["eq"]
; expr = expr_eq
; str_item = str_item_gen_eq
; sig_item = sig_item_gen_eq
})
;

