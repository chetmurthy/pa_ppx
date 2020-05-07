(* camlp5r *)
(* pa_deriving_sexp.ml,v *)
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


module Ctxt = struct
  include Pa_passthru.Ctxt ;

  value plugin_name ctxt = do {
    assert (List.mem_assoc "plugin_name" ctxt.options) ;
    match List.assoc "plugin_name" ctxt.options with [
      <:expr< $str:na$ >> -> na
    | _ -> assert False
    ]}
  ;
  value is_plugin_name ctxt s = (s = plugin_name ctxt) ;

  value is_strict ctxt =
    match List.assoc "strict" ctxt.options with [
      <:expr< True >> -> True
    | <:expr< False >> -> False
    | _ -> failwith "malformed option 'strict'"
    | exception Not_found -> assert False
    ]
  ;

  value is_exn ctxt =
    match List.assoc "exn" ctxt.options with [
      <:expr< True >> -> True
    | <:expr< False >> -> False
    | _ -> failwith "malformed option 'exn'"
    | exception Not_found -> assert False
    ]
  ;
end ;

value tuplepatt loc l = if List.length l = 1 then List.hd l else <:patt< ( $list:l$ ) >> ;
value tupleexpr loc l = if List.length l = 1 then List.hd l else <:expr< ( $list:l$ ) >> ;

value extract_allowed_attribute_expr arg attrna attrs =
  match try_find (fun a -> match uv a with [ <:attribute_body< $attrid:(_, id)$ $exp:e$ ; >>
                   when id = DC.allowed_attribute (DC.get arg) "sexp" attrna ->
                   e
                 | _ -> failwith "extract_allowed_attribute_expr" ]) attrs with [
    e -> Some e
  | exception Failure _ -> None ]
;

module To = struct

type attrmod_t = [ Nobuiltin ] ;

value to_sexp_fname arg tyname =
  (*if tyname = "t" then "sexp_of"
    else *)"sexp_of_"^tyname
;

value to_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = to_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> failwith "cannot derive sexp for type <<_>>"
| <:ctyp:< unit >> -> <:expr< sexp_of_unit >>
| <:ctyp:< int >> -> <:expr< sexp_of_int >>
| <:ctyp:< bool >> -> <:expr< sexp_of_bool >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< sexp_of_int32 >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< sexp_of_int64 >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< sexp_of_string >>
| <:ctyp:< bytes >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_bytes >>
| <:ctyp:< char >> -> <:expr< sexp_of_char >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< sexp_of_nativeint >>
| <:ctyp:< float >> -> <:expr< sexp_of_float >>

| <:ctyp:< Hashtbl.t >> ->
  <:expr< sexp_of_hashtbl >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "sexp_of" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< sexp_of_list $fmt1$ >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< sexp_of_array $fmt1$ >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< sexp_of_ref $fmt1$ >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< sexp_of_option $fmt1$ >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let fmtf = match List.assoc i param_map with [
    x -> x
  | exception Not_found ->
    Ploc.raise loc (Failure "pa_deriving.sexp: unrecognized param-var in type-decl")
  ] in
  <:expr< $lid:fmtf$ >>

| <:ctyp:< $lid:lid$ >> ->
  let fname = to_sexp_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = to_sexp_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record loc arg (uv fields) in

    let conspat = <:patt< $uid:cid$ $recpat$ >> in
    (conspat, <:vala< None >>, <:expr< List [ (Atom $str:jscid$) ;  $body$ ] >>)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> varpats in
    let fmts = List.map fmtrec tyl in

    let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
        fmts vars <:expr< [] >> in

    (conspat, <:vala< None >>, <:expr< List [ (Atom $str:jscid$) :: $liste$ ] >>)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    assert (List.length tyl <= 1) ;
    let tyl = match tyl with [
      [] -> []
    | [<:ctyp< ( $list:l$ ) >>] -> l
    | [t] -> [t]
    | [_::_] -> assert False ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = if varpats = [] then
        <:patt< ` $cid$ >>
      else
        let tuplepat = tuplepatt loc varpats in
        <:patt< ` $cid$ $tuplepat$ >> in
    let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
        fmts vars <:expr< [] >> in
    let liste = <:expr< List [Atom $str:jscid$ :: $liste$] >> in
    (conspat, <:vala< None >>, liste)
  }

  | PvInh _ ty ->
    let lili = match fst (Ctyp.unapplist ty) with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some li, lid)
    | [%unmatched_vala] -> failwith "fmt_expression-PvInh"
     ] in
    let conspat = <:patt< ( # $lilongid:lili$ as z ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< ($fmtf$ z) >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
        fmts vars <:expr< [] >> in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    <:expr< fun ($list:varpats$) -> List $liste$ >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record loc arg fields in
  <:expr< fun $recpat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.to_expression"
| ty ->
  Ploc.raise (loc_of_ctyp ty) (Failure (Printf.sprintf "pa_deriving_sexp.to_expression: %s" (Pp_MLast.show_ctyp ty)))
]
and fmt_record loc arg fields = 
  let labels_vars_fmts_defaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        let attrs = snd(Ctyp.unwrap_attrs ty) in
        let default = extract_allowed_attribute_expr arg "default" attrs in
        let key = extract_allowed_attribute_expr arg "key" attrs in
        let jskey = match key with [
          Some <:expr< $str:k$ >> -> k
        | Some _ -> failwith "@key attribute without string payload"
        | None -> fname ] in
        (fname, Printf.sprintf "v_%s" fname, fmtrec ty, default, jskey)) fields in

  let liste = List.fold_right (fun (f,v,fmtf,dflt, jskey) rhs ->
      match dflt with [
        Some d -> <:expr< let fields = if $lid:v$ = $d$ then fields
                           else [ List [ Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      | None -> <:expr< let fields = [ List [ Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      ]) (List.rev labels_vars_fmts_defaults_jskeys) <:expr< fields >> in
  let liste = <:expr< let fields = [] in $liste$ >> in

  let pl = List.map (fun (f, v, _, _, _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts_defaults_jskeys in
  (<:patt< { $list:pl$ } >>, <:expr< List $liste$ >>)

in fmtrec ty0
;


value fmt_to_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  to_expression arg ~{msg=msg} params t2
| t -> to_expression arg ~{msg} params t
]
;

value str_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = uv tyname in
  let to_sexpfname = to_sexp_fname arg tyname in
  let to_e = fmt_to_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let to_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  (to_sexpfname, Expr.abstract_over paramfun_patts
     <:expr< fun arg -> $to_e$ arg >>)
;

value sig_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = uv tyname in
  let to_sexpfname = to_sexp_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Sexplib.Sexp.t >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Sexplib.Sexp.t >> in
  (to_sexpfname, toftype)
;
end
;

module Of = struct

type attrmod_t = [ Nobuiltin ] ;

value of_sexp_fname arg tyname =
  (*if tyname = "t" then "of_sexp"
    else*) tyname^"_of_sexp"
;

value of_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = of_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< unit >> -> <:expr< unit_of_sexp >>
| <:ctyp:< int >> -> <:expr< int_of_sexp >>
| <:ctyp:< bool >> -> <:expr< bool_of_sexp >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< int32_of_sexp >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< int64_of_sexp >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< string_of_sexp >>
| <:ctyp:< bytes >> -> <:expr< Sexplib0.Sexp_conv.bytes_of_sexp >>
| <:ctyp:< char >> -> <:expr< char_of_sexp >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< nativeint_of_sexp >>
| <:ctyp:< float >> -> <:expr< float_of_sexp >>

| <:ctyp:< Hashtbl.t >> ->
  <:expr< hashtbl_of_sexp >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "of_sexp" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< list_of_sexp $fmt1$ >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< array_of_sexp $fmt1$ >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< ref_of_sexp $fmt1$ >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< option_of_sexp $fmt1$ >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let fmtf = match List.assoc i param_map with [
    x -> x
  | exception Not_found ->
    Ploc.raise loc (Failure "pa_deriving.sexp: unrecognized param-var in type-decl")
  ] in
  <:expr< $lid:fmtf$ >>

| <:ctyp:< $lid:lid$ >> ->
  let fname = of_sexp_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = of_sexp_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record ~{cid=Some cid} loc arg (uv fields) in

    let conspat = <:patt< List [ Atom $str:jscid$ ; $recpat$ ] >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in

    let conspat = List.fold_right (fun v rhs -> <:patt< [ $lid:v$ :: $rhs$ ] >>)
        vars <:patt< [] >> in
    let conspat = <:patt< List [ (Atom $str:jscid$) :: $conspat$ ] >> in

    let varexps = List.map2 (fun fmt v -> <:expr< $fmt$ $lid:v$ >>) fmts vars in
    let consexp = Expr.applist <:expr< $uid:cid$ >> varexps in

    (conspat, <:vala< None >>, consexp)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  let catch_branch = (<:patt< _ >>, <:vala< None >>, <:expr< failwith $str:msg$ >>) in
  let branches = branches @ [catch_branch] in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> as ty0 -> 
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    assert (List.length tyl <= 1) ;
    let tyl = match tyl with [
      [] -> []
    | [<:ctyp< ( $list:l$ ) >>] -> l
    | [t] -> [t]
    | [_::_] -> assert False ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let listpat = List.fold_right (fun vp listpat -> <:patt< [ $vp$ :: $listpat$ ] >>)
        varpats <:patt< [] >> in
    let conspat = <:patt< List [(Atom $str:jscid$) :: $listpat$] >> in
    let consexp = if List.length vars = 0 then
        <:expr< ` $cid$ >>
      else
        let varexps = List.map2 (fun fmt v -> <:expr< $fmt$ $lid:v$ >>) fmts vars in
        let tup = tupleexpr loc varexps in
        <:expr< ` $cid$ $tup$ >> in
    Left (conspat, <:vala< None >>, consexp)
  }

  | PvInh _ ty ->
    let fmtf = fmtrec ty in
    Right fmtf
  ]) l in
  let (lefts,rights) = filter_split isLeft branches in
  let lefts = List.map (mustLeft "of_sexp") lefts in
  let rights = List.map (mustRight "of_sexp") rights in

  let righte = List.fold_right (fun fmtf rhs ->
    <:expr< try ( $fmtf$ sexp :> $ty0$ ) with [ Failure _ -> $rhs$ ] >>)
    rights <:expr< failwith $str:msg$ >> in

  let last_branch = (<:patt< sexp >>, <:vala< None >>,
                     righte) in

  let branches = lefts @ [ last_branch ] in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let listpat = List.fold_right (fun v l -> <:patt< [$v$ :: $l$] >>) varpats <:patt< [] >> in
    let varexps = List.map (fun v -> <:expr< $lid:v$ >>) vars in
    let tuplevars = tupleexpr loc varexps in
    let consexp = <:expr< Result.Ok $tuplevars$ >> in
    let fmts = List.map fmtrec tyl in
    let unmarshe =
      let unmarshexps = List.map2 (fun fmte v -> <:expr< $fmte$ $lid:v$ >>) fmts vars in
      <:expr< ( $list:unmarshexps$ ) >> in
    <:expr< fun [ List $listpat$ -> $unmarshe$
                | _ -> failwith "wrong number of members in list" ] >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record ~{cid=None} loc arg fields in
  <:expr< fun [ $recpat$ -> $body$ | _ -> failwith $str:msg$ ] >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.of_expression"
| ty ->
  Ploc.raise (loc_of_ctyp ty) (Failure (Printf.sprintf "pa_deriving_sexp.of_expression: %s" (Pp_MLast.show_ctyp ty)))
]
and fmt_record ~{cid} loc arg fields = 
  let labels_vars_fmts_defaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        let attrs = snd(Ctyp.unwrap_attrs ty) in
        let default = extract_allowed_attribute_expr arg "default" attrs in
        let key = extract_allowed_attribute_expr arg "key" attrs in
        let jskey = match key with [
          Some <:expr< $str:k$ >> -> k
        | Some _ -> failwith "@key attribute without string payload"
        | None -> fname ] in
        (fname, Printf.sprintf "v_%s" fname, fmtrec ty, default, jskey)) fields in

  let varrow_except (i,iexp) =
    List.mapi (fun j (f,v,fmt,_,_) ->
        if i <> j then <:expr< $lid:v$ >> else iexp)
      labels_vars_fmts_defaults_jskeys in

  let branch1 i (f, v, fmt,_, jskey) =
    let l = varrow_except (i, <:expr< Result.Ok ( $fmt$ $lid:v$ ) >>) in
    let cons1exp = tupleexpr loc l in
    (<:patt< [ List [ Atom $str:jskey$ ; $lid:v$ ] :: xs] >>, <:vala< None >>,
     <:expr< loop xs $cons1exp$ >>) in

  let branches = List.mapi branch1 labels_vars_fmts_defaults_jskeys in

  let finish_branch =
    let recexp =
      let lel = List.map (fun (f,v,_,_,_) -> (<:patt< $lid:f$ >>, <:expr< $lid:v$ >>)) labels_vars_fmts_defaults_jskeys in
      <:expr< { $list:lel$ } >> in
    let consexp = match cid with [ None -> recexp | Some cid -> <:expr< $uid:cid$ $recexp$ >> ] in
    let consexp = <:expr< Result.Ok $consexp$ >> in
    let e = List.fold_right (fun (_,v,_,_,_) rhs -> 
        <:expr< Rresult.R.bind $lid:v$ (fun $lid:v$ -> $rhs$) >>) labels_vars_fmts_defaults_jskeys consexp in
    (<:patt< [] >>, <:vala< None >>, e) in

  let catch_branch =
    if Ctxt.is_strict arg then
      (<:patt< [_ :: _] >>, <:vala< None >>, <:expr< failwith $str:msg$ >>)
  else
    let varrow = varrow_except (-1, <:expr< . >>) in
    let cons1exp = tupleexpr loc varrow in
    (<:patt< [_ :: xs] >>, <:vala< None >>, <:expr< loop xs $cons1exp$ >>) in

  let branches = branches @ [finish_branch; catch_branch] in

  let e = 
    let varpats = List.map (fun (_,v,_,_,_) -> <:patt< $lid:v$ >>) labels_vars_fmts_defaults_jskeys in
    let tuplevars = tuplepatt loc varpats in
    let initexps = List.map (fun (f,_,_,dflt,_) ->
        match dflt with [
          None ->
          let msg = msg^"."^f in
          <:expr< Result.Error $str:msg$ >>
        | Some d -> <:expr< Result.Ok $d$ >> ]) labels_vars_fmts_defaults_jskeys in
    let tupleinit = tupleexpr loc initexps in
    <:expr< let rec loop xs $tuplevars$ = match xs with [ $list:branches$ ]
            in match loop xs $tupleinit$ with [ Result.Ok r -> r | Result.Error msg -> failwith msg ] >> in

  (<:patt< List xs >>, e)

in fmtrec ty0
;

value fmt_of_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  of_expression arg ~{msg=msg} params t2
| t -> of_expression arg ~{msg=msg} params t
]
;

value str_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = uv tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  let paramfun_exprs = List.map (fun (_,ppf) -> <:expr< $lid:ppf$ >>) param_map in
  let body = fmt_of_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let e = 
    let of_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $body$ >> in
    (of_sexpfname, Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $of_e$ arg >>) in
  [e]
;

value sig_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = uv tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Sexplib.Sexp.t -> $pty$ >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Sexplib.Sexp.t -> $(Ctyp.applist ty paramtys)$ >> in
  let e = (of_sexpfname, offtype) in
  [e]
;

end
;

value str_item_top_funs arg tyname param_map ty =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
    [To.str_item_top_funs arg tyname param_map ty]
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.str_item_top_funs arg tyname param_map ty
   else [])
;

value sig_item_top_funs arg tyname param_map ty =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
    [To.sig_item_top_funs arg tyname param_map ty]
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.sig_item_top_funs arg tyname param_map ty
   else [])
;

value str_item_funs arg ((loc,_) as tyname) params ty =
  let param_map = List.mapi (fun i p ->
    match uv (fst p) with [
      None -> Ploc.raise loc (Failure "cannot derive sexp-functions for type decl with unnamed type-vars")
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params in
  let l = str_item_top_funs arg tyname param_map ty in
  if l = [] then Ploc.raise loc (Failure "no functions generated") else
  let types = sig_item_top_funs arg tyname param_map ty in
  List.map (fun (fname, body) ->
      let fty = List.assoc fname types in
      let fty = if param_map = [] then fty
        else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >> in
      (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [] >>)) l
;

value sig_item_funs arg ((loc,_) as tyname) params ty =
  let param_map = List.mapi (fun i p ->
    match uv (fst p) with [
      None -> failwith "cannot derive sexp-functions for type decl with unnamed type-vars"
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params in
  let l = sig_item_top_funs arg tyname param_map ty in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_sexp0 arg td =
  let tyname = uv td.tdNam
  and params = uv td.tdPrm
  and tk = td.tdDef in
  str_item_funs arg tyname params tk
;

value loc_of_type_decl td = fst (uv td.tdNam) ;

value str_item_gen_sexp name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_sexp0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_sexp0 arg td =
  let tyname = uv td.tdNam
  and params = uv td.tdPrm
  and tk = td.tdDef in
  sig_item_funs arg tyname params tk
;

value sig_item_gen_sexp name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_item_gen_sexp0 arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

value type_params t =
  let acc = ref [] in
  let add1 tv = if not (List.mem tv acc.val) then Std.push acc tv else () in
  let rec brec = fun [
    <:ctyp< ' $tv$ >> -> add1 tv
  | <:ctyp< $a$ $b$ >> -> do { brec a; brec b }
  | <:ctyp< ( $list:l$ ) >> -> List.iter brec l
  | <:ctyp< [= $list:branches$ ] >> ->
    List.iter (fun [ PvTag _ _ _ tyl _ -> List.iter brec (uv tyl) | PvInh _ ty -> brec ty ])  branches
  | _ -> ()
  ] in do {
    brec t ; List.rev acc.val
  }
;

value build_param_map t =
  let pvl = type_params t in
  List.mapi (fun i v -> (v, Printf.sprintf "tp_%d" i)) pvl
;

value expr_sexp arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "sexp_of" || id = "derive.sexp_of" ->
    let loc = loc_of_ctyp ty in
    let param_map = build_param_map ty in
    let e = To.fmt_to_top arg ~{msg=Printf.sprintf "%s.sexp_of"  (Ctxt.module_path_s arg)} param_map ty in
    let e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $e$ >> in
    let parampats = List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map in
    Expr.abstract_over parampats e

| <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_sexp" || id = "derive.of_sexp" ->
    let loc = loc_of_ctyp ty in
    let param_map = build_param_map ty in
    let e = Of.fmt_of_top ~{msg=Printf.sprintf "%s.of_sexp"  (Ctxt.module_path_s arg)} arg param_map ty in
    let e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $e$ >> in
    let parampats = List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map in
    Expr.abstract_over parampats e
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "sexp"
; alternates = ["of_sexp"; "sexp_of"; "sexp_poly"]
; options = ["optional"; "strict"; "exn"]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>); ("strict", <:expr< False >>); ("exn", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"; "key"; "name"; "encoding"; "default"; "sexp_of"; "of_sexp"]
; expr_extensions = ["of_sexp"; "sexp_of"]
; expr = expr_sexp
; str_item = str_item_gen_sexp
; sig_item = sig_item_gen_sexp
})
;

