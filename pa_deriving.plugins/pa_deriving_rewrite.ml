(* camlpg5r *)
(* pa_deriving_rewrite.ml,v *)
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
open Pa_ppx_utils ;

value rewrite_fname arg tyname =
  if tyname = "t" then "rewrite"
  else "rewrite_"^tyname
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc ty = assert False ; end) ;

value fmt_expression arg ?{coercion} param_map ty0 =
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [
    <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = rewrite_fname arg lid in
  <:expr< $lid:fname$ >>

  | <:ctyp:< _ >> -> <:expr< fun x -> x >>
  | <:ctyp:< unit >> -> <:expr< fun x -> x >>
  | <:ctyp:< int >> -> <:expr< fun x -> x >>
  | <:ctyp:< int32 >> -> <:expr< fun x -> x >>
  | <:ctyp:< int64 >> -> <:expr< fun x -> x >>
  | <:ctyp:< nativeint >> -> <:expr< fun x -> x >>
  | <:ctyp:< float >> -> <:expr< fun x -> x >>
  | <:ctyp:< bool >> -> <:expr< fun x -> x >>
  | <:ctyp:< char >> -> <:expr< fun x -> x >>
  | <:ctyp:< string >> -> <:expr< fun x -> x >>
  | <:ctyp:< bytes >> -> <:expr< fun x -> x >>

  | <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> List.map $fmt1$ a >>

  | <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> Array.map $fmt1$ a >>

  | (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
    let fmt1 = fmtrec ty in
    <:expr< fun a -> ref ($fmt1$ a.val) >>

  | <:ctyp:< lazy_t $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [ lazy x ->  lazy ( $fmt1$ x ) ] >>

  | <:ctyp:< option $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [
            None -> None
          | (Some a) -> Some ($fmt1$ a) ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun [
          (Result.Ok a) -> Result.Ok ($(fmtrec ty1)$ a)
        | (Result.Error a) -> Result.Error ($(fmtrec ty2)$ a)
      ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.rewrite: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when Some id = DC.allowed_attribute (DC.get arg) "rewrite" "nobuiltin" ->
  fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec t

| <:ctyp:< $lid:lid$ >> ->
  let fname = rewrite_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = rewrite_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v,_) -> <:patt< $lid:v$ >>) vars_fmts in

    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = <:expr< ( $list:fldmaps$ ) >> in

    <:expr< fun ( $list:var1pats$ ) -> $cmpexp$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    <:constructor:< $uid:cid$ of { $list:fields$ } $algattrs:_$ >> ->
    let (rec1pat, body) = fmt_record loc arg fields in

    let conspat = <:patt< ($uid:cid$ $rec1pat$) >> in
    (conspat, <:vala< None >>, <:expr< $uid:cid$ $body$ >>)

  | <:constructor:< $uid:cid$ of $list:tyl$ $algattrs:_$ >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var1pats in    
    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = Expr.applist <:expr< $uid:cid$ >> fldmaps in

    (conspat, <:vala< None >>, cmpexp)

  | (_, _, _, <:vala< Some _ >>, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>


| <:ctyp:< [= $list:l$ ] >> as z ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> var1pats in

    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = Expr.applist <:expr< ` $cid$  >> fldmaps in
    let cmpexp = <:expr< ( $cmpexp$ :> $z$ ) >> in

    (conspat, <:vala< None >>, cmpexp)

  | PvInh _ ty ->
    let lili = match fst (Ctyp.unapplist ty) with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some <:vala< li >>, lid)
    ] in
    let conspat = <:patt< ( # $lilongid:lili$ as a ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< ( ( $fmtf$ a ) :> $z$ ) >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, body) = fmt_record loc arg fields in
  let rec1pat = match coercion with [ None -> rec1pat | Some ty -> <:patt< ( $rec1pat$ : $ty$ ) >> ] in
  <:expr< fun $rec1pat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_rewrite.fmt_expression"
  ]
  and fmt_record loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "a_%s" fname, fmtrec ty)) fields in

  let v1_pl = List.map (fun (f, v,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v1pat = <:patt< { $list:v1_pl$ } >> in
  let fldmaps = List.map (fun (v, v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) labels_vars_fmts in
  let flds = List.map2 (fun (f,_) e -> (f,e)) v1_pl fldmaps in 
  let cmpexp = <:expr< { $list:flds$ } >>
  in
  (v1pat, cmpexp)
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
  let param_map = PM.make "rewrite" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let coercion =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    monomorphize_ctyp (Ctyp.applist ty paramtys) in
  let eqfname = rewrite_fname arg tyname in
  let e = fmt_top arg ~{coercion=coercion} param_map ty in

  let paramfun_patts = List.map (PM.arg_patt ~{naked=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  [(eqfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg -> $e$ arg >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "rewrite" loc (uv td.tdPrm) in
  let tk = td.tdDef in
  let tyname = uv tyname in
  let rewritefname = rewrite_fname arg tyname in
  let paramvars1 = List.map (fun p -> (PM.type_id p)^"_1") param_map in
  let paramvars2 = List.map (fun p -> (PM.type_id p)^"_2") param_map in
  let paramtys1 = List.map (fun tyna -> <:ctyp< ' $tyna$ >>) paramvars1 in
  let paramtys2 = List.map (fun tyna -> <:ctyp< '$tyna$ >>) paramvars2 in
  let argfmttys = List.map2 (fun pty1 pty2 -> <:ctyp< $pty1$ -> $pty2$ >>) paramtys1 paramtys2 in  
  let basety = <:ctyp< $lid:tyname$ >> in
  let thety1 = Ctyp.applist basety paramtys1 in
  let thety2 = Ctyp.applist basety paramtys2 in
  let rewriteftype = Ctyp.arrows_list loc argfmttys <:ctyp< $thety1$ -> $thety2$  >> in
  let rewriteftype_constraint =
    if is_poly_variant tk then rewriteftype
    else
      <:ctyp< ! $list:paramvars1@paramvars2$ . $rewriteftype$ >> in
  [(rewritefname, (rewriteftype, rewriteftype_constraint))]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "rewrite" loc (uv td.tdPrm) in
  let l = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  List.map (fun (fname, body) ->
      let (fty, fty_constraint) = List.assoc fname types in
      let fty = if param_map = [] then fty
        else fty_constraint in
      let attrwarn39 = <:attribute_body< "ocaml.warning" "-39" ; >> in
      let attrwarn39 = <:vala< attrwarn39 >> in
      (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [attrwarn39] >>)) l
;

module Dispatch1 = struct
type tyarg_t = {
  name: string
; srctype : ctyp
; dsttype : ctyp
; subs : list (ctyp * ctyp)
} ;
type t = (string * tyarg_t) ;

value tyvars t =
  let rec trec acc = fun [
    <:ctyp< $t1$ $t2$ >> -> trec (trec acc t1) t2
  | <:ctyp< ' $id$ >> -> [ id :: acc ]
  | _ -> acc
  ] in
  Std.uniquize(trec [] t)
;

value to_type (_, t) =
  let loc = loc_of_ctyp t.srctype in
  let vars = Std.uniquize((tyvars t.srctype)@(tyvars t.dsttype)@
                          List.concat (List.map (fun (a,b) -> (tyvars a)@(tyvars b)) t.subs)) in
  let rhs = <:ctyp< rewriter_t $t.srctype$ $t.dsttype$ >> in
  let rhs = List.fold_right (fun (a,b) rhs -> <:ctyp< (rewriter_t $a$ $b$) -> $rhs$ >>) t.subs rhs in
  if vars = [] then rhs else
  <:ctyp< ! $list:vars$ . $rhs$ >>
;

value convert_subs loc e =
  let rec crec acc = fun [
    <:expr< [] >> -> List.rev acc
  | <:expr< [ ( [%typ: $type:t1$], [%typ: $type:t2$] ) :: $tl$ ] >> ->
    crec [ (t1, t2) :: acc ] tl
  ] in
  crec [] e
;
value convert_tyarg loc name tyargs =
  let alist = List.map (fun [
      (<:patt< $lid:id$ >>, e) -> (id, e)
    | _ -> Ploc.raise loc (Failure "bad tyarg label -- must be lident")
    ]) tyargs in
  let srctype = match List.assoc "srctype" alist with [
    <:expr< [%typ: $type:t$] >> -> t
  | _ -> Ploc.raise loc (Failure "bad tyarg rhs -- must be [%typ: type]")
  | exception Not_found -> Ploc.raise loc (Failure "missing srctype tyarg")
  ] in
  let dsttype = match List.assoc "dsttype" alist with [
    <:expr< [%typ: $type:t$] >> -> t
  | _ -> Ploc.raise loc (Failure "bad tyarg rhs -- must be [%typ: type]")
  | exception Not_found -> Ploc.raise loc (Failure "missing dsttype tyarg")
  ] in
  let subs = match List. assoc "subs" alist with [
    <:expr:< [ $_$ :: $_$ ] >> as z -> convert_subs loc z
  | _ -> Ploc.raise loc (Failure "bad tyarg rhs -- must be a list")
  | exception Not_found -> []
  ] in
  { name = name ; srctype = srctype ; dsttype = dsttype ; subs = subs }
;
value convert loc (fname, tyargs) =
  (fname, convert_tyarg loc fname tyargs)
;

value expr_wrap_dsttype_module d e =
  match Ctyp.unapplist d.dsttype with [
    (<:ctyp< $lid:_$ >>, _) -> e
  | (<:ctyp:< $longid:li$ . $lid:_$ >>, _) -> <:expr< let open $module_expr_of_longident li$ in $e$ >>
  | _ -> Ploc.raise (loc_of_ctyp d.dsttype) (Failure "expr_wrap_dsttype_module: malformed dsttype")
  ]
;

value patt_wrap_dsttype_module d p =
  match Ctyp.unapplist d.dsttype with [
    (<:ctyp< $lid:_$ >>, _) -> p
  | (<:ctyp:< $longid:li$ . $lid:_$ >>, _) -> <:patt< $longid:li$ . $p$ >>
  | _ -> Ploc.raise (loc_of_ctyp d.dsttype) (Failure "patt_wrap_dsttype_module: malformed dsttype")
  ]
;

end
;

module Rewrite = struct

type t = {
  dispatch_type_name : string
; dispatchers : list Dispatch1.t
; type_decls : list (string * MLast.type_decl)
} ;

value dispatch_table_type_decls loc t =
  let ltl = List.map (fun (dispatcher_name, t) ->
    let ty = Dispatch1.to_type (dispatcher_name, t) in
    (loc_of_ctyp ty, dispatcher_name, False, ty, <:vala< [] >>)
  ) t.dispatchers in
  let dispatch_table_type = <:ctyp< { $list:ltl$ } >> in
  [ <:type_decl< $lid:t.dispatch_type_name$ = $dispatch_table_type$ >> ;
    <:type_decl< rewriter_t 'a 'b = $lid:t.dispatch_type_name$ -> 'a -> 'b >> ]
;

value build_context loc ctxt tdl =
  let open Ctxt in
  let dispatch_type_name = match option ctxt "dispatch_type" with [
      <:expr< $lid:id$ >> -> id
    | _ -> Ploc.raise loc (Failure "pa_deriving.rewrite: must specify option dispatch_type")
  ] in
  let dispatchers = match option ctxt "dispatchers" with [
    <:expr:< { $list:lel$ } >> ->
      List.map (fun [
          (<:patt< $lid:fname$ >>, <:expr:< { $list:tyargs$ } >>) ->
          (Dispatch1.convert loc (fname, tyargs))
        | _ -> Ploc.raise loc (Failure "pa_deriving.rewrite: malformed dispatcher args")
      ]) lel
  ] in
  let type_decls = List.map (fun (MLast.{tdNam=tdNam} as td) ->
      (tdNam |> uv |> snd |> uv, td)
    ) tdl in
  {
    dispatch_type_name = dispatch_type_name;
    dispatchers = dispatchers ;
    type_decls = type_decls
  }
;

value reduce1 (id, tyargs) td = do {
  if List.length tyargs <> List.length (uv td.tdPrm) then
    Ploc.raise (loc_of_type_decl td) (Failure "actual/formal length mismatch")
  else () ;
  let rho = List.map2 (fun formal actual ->
      match formal with [
        ( <:vala< Some tyv >>, _ ) -> (tyv, actual)
      | _ -> Ploc.raise (loc_of_type_decl td) (Failure "pa_deriving.rewrite: blank formal type-variables are not supported")
      ]
    ) (uv td.tdPrm) tyargs in
  let rho = Std.filter (fun [
      (id, <:ctyp< ' $id2$ >>) when id = id2 -> False
    | _ -> True
    ]) rho in
  let rhs = match td.tdDef with [
    <:ctyp< $t1$ == $t2$ >> -> t2
  | t -> t
  ] in
  if rho = [] then rhs else
    Ctyp.subst rho rhs
}
;

value head_reduce1 t ty =
  match Ctyp.unapplist ty with [
    (<:ctyp< $lid:id$ >>, tyargs) when List.mem_assoc id t.type_decls ->
    let td = List.assoc id t.type_decls in
    reduce1 (id, tyargs) td
  | _ -> ty
  ]
;

value pmatch pat ty =
  let rec pmrec acc = fun [
    (<:ctyp< $lid:id$ >>, <:ctyp< $lid:id2$ >>) when id = id2 -> acc
  | (<:ctyp< $p1$ $p2$ >>, <:ctyp< $t1$ $t2$ >>) ->
    pmrec (pmrec acc (p1, t1)) (p2, t2)
  | (<:ctyp< ' $id$ >>, ty) ->
    if List.mem_assoc id acc then
      Ploc.raise (loc_of_ctyp pat) (Failure "polymorphic type-variables in patterns must not be repeated")
    else
      [ (id, ty) :: acc ]
  | _ -> failwith "caught"
  ]
  in match pmrec [] (pat, ty) with [
    rho -> Some rho
  | exception Failure _ -> None
  ]
;

value match_rewrite_rule ~{except} t ctyp =
  List.find_map (fun (dname, t) ->
    if (Some dname) = except then None else
     ctyp
     |> pmatch t.Dispatch1.srctype
     |> Std.map_option (fun rho -> (t, rho))
  ) t.dispatchers
;

(** strategy for generating a rewriter.

(1) start with srctype

(2) reduce it; if you get no change, it's a failure

(3) if it matches anything other than the current rewrite-dispatcher rule, apply that.

(4) otherwise, keep reducing until you get TySum or TyRec

(5) Take the dsttype's module-prefix and use it

(6) And generate a copy-expression

*)

value rec match_or_head_reduce ~{except} t ty =
  match (except, match_rewrite_rule ~{except=except} t ty) with [
    (_, Some (d, rho)) -> Left (d, rho)
  | (Some dname, None) ->
    let ty' = head_reduce1 t ty in
    if Reloc.eq_ctyp ty ty' then
      match ty with [
        (<:ctyp< [ $list:_$ ] >> | <:ctyp< { $list:_$ } >>) -> Right (dname, ty)

      | _ -> Ploc.raise (loc_of_ctyp ty) (Failure Printf.(sprintf "rewrite rule %s: cannot rewrite srctype" dname))
      ]
    else
      match_or_head_reduce ~{except=except} t ty'
  | (None, None) ->
    Ploc.raise (loc_of_ctyp ty) (Failure "match_or_head_reduce: cannot head-reduce except at toplevel of a dispatcher's srctype")
  ]
;

value generate_leaf_dispatcher_expression t d = fun [
  <:ctyp:< [ $list:branches$ ] >> ->
  let l = List.map (fun [
      <:constructor< $uid:uid$ of $list:tyl$ >> ->
      let argvars = List.mapi (fun i _ -> Printf.sprintf "v_%d" i) tyl in
      let patt = List.fold_left (fun p v -> <:patt< $p$ $lid:v$ >>) <:patt< $uid:uid$ >> argvars in
      let expr = List.fold_left (fun e v -> <:expr< $e$ $lid:v$ >>) <:expr< $uid:uid$ >> argvars in
      (patt, <:vala< None >>, Dispatch1.expr_wrap_dsttype_module d expr)
    ]) branches in
  <:expr< fun [ $list:l$ ] >>
| <:ctyp:< { $list:ltl$ } >> ->
    let patt =
      let lpl = List.mapi (fun i (_, lid, _, _, _) ->
          (<:patt< $lid:lid$ >>, <:patt< $lid:lid$ >>)
        ) ltl in
      <:patt< { $list:lpl$ } >> in
    let expr =
      let lel = List.mapi (fun i (_, lid, _, _, _) ->
          (Dispatch1.patt_wrap_dsttype_module d <:patt< $lid:lid$ >>, <:expr< $lid:lid$ >>)
        ) ltl in
      <:expr< { $list:lel$ } >> in
    <:expr< fun [ $patt$ -> $expr$ ] >>
]
;

value rec generate_dispatcher_expression ~{except} t ty = 
  match match_or_head_reduce ~{except=except} t ty with [
    Left (rwd, lrho) ->
    (** [rwd] is the rewrite dispatcher that matched,
        and [lrho] is the substitution generated by the match. *)
    let (revsubs, rrho) = List.fold_left (fun (revsubs, rrho) (lhsty, rhsty) ->
        let conc_lhsty = Ctyp.subst lrho lhsty in
        let (e, conc_rhsty) = generate_dispatcher_expression ~{except=None} t conc_lhsty in
        let add_rrho = match pmatch rhsty conc_rhsty with [
          None -> Ploc.raise (loc_of_ctyp ty) (Failure "generate_dispatcher_expression: subterm dispatch returned non-matching type")
        | Some rho -> rho
        ] in
        ([ e :: revsubs ], Ctyp.append_rho rrho add_rrho)
      ) ([], []) rwd.Dispatch1.subs in
    let dname = rwd.Dispatch1.name in
    let loc = loc_of_ctyp ty in
    let e = Expr.applist <:expr< $lid:dname$ >> (List.rev revsubs) in
    (e, Ctyp.subst rrho rwd.Dispatch1.dsttype)

  | Right (dname, headredty) ->
    let d = List.assoc dname t.dispatchers in
    (generate_leaf_dispatcher_expression t d headredty, d.Dispatch1.dsttype)
  ]
;

value generate_dispatcher t (dname,d) = 
  let srctype = d.Dispatch1.srctype in
  generate_dispatcher_expression ~{except=Some dname} t srctype
;
end ;

value sig_items arg td =
  let loc = loc_of_type_decl td in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, (ty,_)) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_rewrite0 arg td =
  str_item_funs arg td
;

value str_item_gen_rewrite name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let rc = Rewrite.build_context loc arg tdl in
    let dispatch_type_decls = Rewrite.dispatch_table_type_decls loc rc in
    let rewrite_dispatcher_decls = List.map (fun (dname,d) ->
        let (e,ty) = Rewrite.generate_dispatcher rc (dname, d) in
        let loc = loc_of_expr e in
        let e = <:expr< ( $e$ : $ty$ ) >> in
        (<:patt< $lid:dname$ >>, e, <:vala< [] >>)
      ) rc.Rewrite.dispatchers in
    let si = <:str_item< value rec $list:rewrite_dispatcher_decls$ >> in
(*
    let l = List.concat (List.map (str_item_gen_rewrite0 arg) tdl) in
    let si = <:str_item< value rec $list:l$ >> in
*)  
  <:str_item< declare type $list:dispatch_type_decls$ ; si ; end >>
| _ -> assert False ]
;

value sig_item_gen_rewrite name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "rewrite"
; alternates = []
; options = ["optional"; "dispatchers"; "dispatch_type"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_rewrite
; sig_item = sig_item_gen_rewrite
})
;

