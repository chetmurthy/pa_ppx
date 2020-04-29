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
  match try_find (fun a -> match Pcaml.unvala a with [ <:attribute_body< $attrid:(_, id)$ $exp:e$ ; >>
                   when id = DC.allowed_attribute (DC.get arg) "sexp" attrna ->
                   e
                 | _ -> failwith "extract_allowed_attribute_expr" ]) attrs with [
    e -> Some e
  | exception Failure _ -> None ]
;

module To = struct

type attrmod_t = [ Nobuiltin ] ;

value to_sexp_fname arg tyname =
  if tyname = "t" then "to_sexp"
  else tyname^"_to_sexp"
;

value to_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = to_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> failwith "cannot derive sexp for type <<_>>"
| <:ctyp:< Sexp.Safe.t >> -> <:expr< fun x -> x >>
| <:ctyp:< unit >> -> <:expr< fun () -> `Null >>
| <:ctyp:< int >> -> <:expr< fun x -> `Int x >>
| <:ctyp:< bool >> -> <:expr< fun x -> `Bool x >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< fun x -> `Intlit (Int32.to_string x) >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< fun x -> `Intlit (Int64.to_string x) >>
| <:ctyp:< int64 [@encoding `string ; ] >> | <:ctyp:< Int64.t [@encoding `string ; ] >> ->
    <:expr< fun x -> `String (Int64.to_string x) >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< fun x -> `String x >>
| <:ctyp:< bytes >> -> <:expr< fun x -> `String (Bytes.to_string x) >>
| <:ctyp:< char >> -> <:expr< fun x -> `String (String.make 1 x) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< fun x -> `Intlit (Nativeint.to_string x) >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> ->
    <:expr< fun x -> `String (Nativeint.to_string x) >>
| <:ctyp:< float >> -> <:expr< fun x -> `Float x >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "to_sexp" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun x ->
        `List (safe_map $fmt1$ x) >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun x ->
        `List
          (Array.to_list
             (Array.map $fmt1$ x)) >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< fun x -> $fmt1$ x.val >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun [
        None -> `Null
      | Some x -> $fmt1$ x ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let fmtf = match List.assoc i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.sexp: unrecognized param-var in type-decl"
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
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record loc arg (Pcaml.unvala fields) in

    let conspat = <:patt< $uid:cid$ $recpat$ >> in
    (conspat, <:vala< None >>, <:expr< `List [ (`String $str:jscid$) ;  $body$ ] >>)

  | (loc, cid, tyl, None, attrs) ->
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = Pcaml.unvala tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> varpats in
    let fmts = List.map fmtrec tyl in

    let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
        fmts vars <:expr< [] >> in

    (conspat, <:vala< None >>, <:expr< `List [ (`String $str:jscid$) :: $liste$ ] >>)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = Pcaml.unvala tyl in
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
    let liste = <:expr< `List [`String $str:jscid$ :: $liste$] >> in
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
    <:expr< fun ($list:varpats$) -> `List ($liste$) >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record loc arg fields in
  <:expr< fun $recpat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.to_expression"
]
and fmt_record loc arg fields = 
  let labels_vars_fmts_defaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (Pcaml.unvala attrs) in
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
                           else [($str:jskey$, $fmtf$ $lid:v$) :: fields ] in $rhs$ >>
      | None -> <:expr< let fields = [($str:jskey$, $fmtf$ $lid:v$) :: fields ] in $rhs$ >>
      ]) (List.rev labels_vars_fmts_defaults_jskeys) <:expr< fields >> in
  let liste = <:expr< let fields = [] in $liste$ >> in

  let pl = List.map (fun (f, v, _, _, _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts_defaults_jskeys in
  (<:patt< { $list:pl$ } >>, <:expr< `Assoc $liste$ >>)

in fmtrec ty0
;


value fmt_to_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  to_expression arg ~{msg=msg} params t2
| t -> to_expression arg ~{msg} params t
]
;

value str_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in
  let to_sexpfname = to_sexp_fname arg tyname in
  let to_e = fmt_to_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let to_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  (to_sexpfname, Expr.abstract_over paramfun_patts
     <:expr< fun arg -> $to_e$ arg >>)
;

value sig_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in
  let to_sexpfname = to_sexp_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Sexp.Safe.t >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Sexp.Safe.t >> in
  (to_sexpfname, toftype)
;
end
;

module Of = struct

type attrmod_t = [ Nobuiltin ] ;

value of_sexp_fname arg tyname =
  if tyname = "t" then "of_sexp"
  else tyname^"_of_sexp"
;

value of_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = of_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< Sexp.Safe.t >> -> <:expr< fun x -> Result.Ok x >>
| <:ctyp:< unit >> -> <:expr< fun [ `Null -> Result.Ok () | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int >> -> <:expr< fun [`Int x -> Result.Ok x | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< bool >> -> <:expr< fun [
        `Bool x -> Result.Ok x
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< fun [
        `Int x -> Result.Ok (Int32.of_int x)
      | `Intlit x -> Result.Ok (Int32.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< fun [
      `Int x -> Result.Ok (Int64.of_int x)
      | `Intlit x -> Result.Ok (Int64.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int64 [@encoding `string ; ] >> | <:ctyp:< Int64.t [@encoding `string ; ] >> ->
 <:expr< fun [
        `String x -> Result.Ok (Int64.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< fun [
        `String x -> Result.Ok x
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< bytes >> -> <:expr< fun [
        `String x -> Result.Ok (Bytes.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< char >> -> <:expr< fun [ `String x ->
          if (String.length x) = 1
          then Result.Ok (x.[0])
          else Result.Error $str:msg$
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< fun [
        `Int x -> Result.Ok (Nativeint.of_int x)
      | `Intlit x -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> -> <:expr< fun [
        `String x -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< float >> -> <:expr< fun [
        `Int x -> Result.Ok (float_of_int x)
      | `Intlit x -> Result.Ok (float_of_string x)
      | `Float x -> Result.Ok x
      | _ -> Result.Error $str:msg$ ] >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "sexp" "of_sexp" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun [
        `List xs ->
          Pa_ppx_runtime.map_bind
            $fmt1$ [] xs
      | _ -> Result.Error $str:msg$ ] >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun [
        `List xs ->
          Rresult.R.bind (Pa_ppx_runtime.map_bind
             $fmt1$ [] xs)
             (fun x -> Result.Ok (Array.of_list x))
      | _ -> Result.Error $str:msg$ ] >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< fun x ->
        Rresult.R.bind ($fmt1$ x) (fun x -> Result.Ok (ref x)) >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< (fun [
        `Null -> Result.Ok None
      | x ->
          Result.bind ($fmt1$ x) (fun x -> Result.Ok (Some x)) ]) >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let fmtf = match List.assoc i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.sexp: unrecognized param-var in type-decl"
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
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record ~{cid=Some cid} loc arg (Pcaml.unvala fields) in

    let conspat = <:patt< `List [ `String $str:jscid$ ; $recpat$ ] >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = Pcaml.unvala tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varexps = List.map (fun v -> <:expr< $lid:v$ >>) vars in
    let fmts = List.map fmtrec tyl in

    let conspat = List.fold_right (fun v rhs -> <:patt< [ $lid:v$ :: $rhs$ ] >>)
        vars <:patt< [] >> in
    let conspat = <:patt< `List [ (`String $str:jscid$) :: $conspat$ ] >> in

    let consexp = Expr.applist <:expr< $uid:cid$ >> varexps in
    let consexp = <:expr< Result.Ok $consexp$ >> in
    let rhs = List.fold_right2 (fun v fmt rhs ->
        <:expr< Rresult.R.bind ($fmt$ $lid:v$) (fun $lid:v$ -> $rhs$) >>)
        vars fmts consexp in

    (conspat, <:vala< None >>, rhs)

  | (_, _, _, Some _, _) -> assert False
  ]) l in
  let catch_branch = (<:patt< _ >>, <:vala< None >>, <:expr< Result.Error $str:msg$ >>) in
  let branches = branches @ [catch_branch] in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> as ty0 -> 
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = Pcaml.unvala cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (Pcaml.unvala attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = Pcaml.unvala tyl in
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
    let conspat = <:patt< `List [(`String $str:jscid$) :: $listpat$] >> in
    let consexp = if List.length vars = 0 then
        <:expr< ` $cid$ >>
      else
        let varexps = List.map (fun v -> <:expr< $lid:v$ >>) vars in
        let tup = tupleexpr loc varexps in
        <:expr< ` $cid$ $tup$ >> in
    let consexp = <:expr< Result.Ok $consexp$ >> in
    let unmarshe = List.fold_right2 (fun fmte v rhs ->
        <:expr< Rresult.R.bind ($fmte$ $lid:v$) (fun $lid:v$ -> $rhs$) >>) fmts vars consexp in

    Left (conspat, <:vala< None >>, unmarshe)
  }

  | PvInh _ ty ->
    let fmtf = fmtrec ty in
    Right fmtf
  ]) l in
  let (lefts,rights) = filter_split isLeft branches in
  let lefts = List.map (mustLeft "of_sexp") lefts in
  let rights = List.map (mustRight "of_sexp") rights in

  let righte = List.fold_right (fun fmtf rhs ->
    <:expr< match $fmtf$ json with [
                     Result.Ok result -> Result.Ok (result :> $ty0$)
                   | Result.Error _ -> $rhs$ ] >>)
    rights <:expr< Result.Error $str:msg$ >> in

  let last_branch = (<:patt< json >>, <:vala< None >>,
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
    let unmarshe = List.fold_right2 (fun fmte v rhs ->
        <:expr< Rresult.R.bind ($fmte$ $lid:v$) (fun $lid:v$ -> $rhs$) >>) fmts vars consexp in
    <:expr< fun [ `List $listpat$ -> $unmarshe$
                | _ -> Result.Error $str:msg$ ] >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record ~{cid=None} loc arg fields in
  <:expr< fun [ $recpat$ -> $body$ | _ -> Result.Error $str:msg$ ] >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.of_expression"
]
and fmt_record ~{cid} loc arg fields = 
  let labels_vars_fmts_defaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (Pcaml.unvala attrs) in
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
    let l = varrow_except (i, <:expr< $fmt$ $lid:v$ >>) in
    let cons1exp = tupleexpr loc l in
    (<:patt< [($str:jskey$, $lid:v$) :: xs] >>, <:vala< None >>,
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
      (<:patt< [_ :: _] >>, <:vala< None >>, <:expr< Result.Error $str:msg$ >>)
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
            in loop xs $tupleinit$ >> in

  (<:patt< `Assoc xs >>, e)

in fmtrec ty0
;

value fmt_of_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  of_expression arg ~{msg=msg} params t2
| t -> of_expression arg ~{msg=msg} params t
]
;

value str_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  let paramfun_exprs = List.map (fun (_,ppf) -> <:expr< $lid:ppf$ >>) param_map in
  let body = fmt_of_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let e = 
    let of_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $body$ >> in
    (of_sexpfname, Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $of_e$ arg >>) in
  if not (Ctxt.is_exn arg) then [e] else
    let exn_name = of_sexpfname^"_exn" in
    let body = Expr.applist <:expr< $lid:of_sexpfname$ >> paramfun_exprs in
    let body = <:expr< match $body$ arg with [ Rresult.Ok x -> x | Result.Error s -> failwith s ] >> in
    let of_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $body$ >> in
    let e' = (exn_name, Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $of_e$ >>) in
    [e; e']
;

value sig_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Sexp.Safe.t -> Rresult.result $pty$ string >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Sexp.Safe.t -> Rresult.result $(Ctyp.applist ty paramtys)$ string >> in
  let e = (of_sexpfname, offtype) in
  if not (Ctxt.is_exn arg) then [e] else
  let exn_name = of_sexpfname^"_exn" in
  let exnftype = Ctyp.arrows_list loc argfmttys <:ctyp< Sexp.Safe.t -> $(Ctyp.applist ty paramtys)$ >> in
  let e' = (exn_name, exnftype) in
  [e; e']
;

end
;

value str_item_top_funs arg tyname param_map ty =
  (if Ctxt.is_plugin_name arg "to_sexp" || Ctxt.is_plugin_name arg "sexp" then
    [To.str_item_top_funs arg tyname param_map ty]
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.str_item_top_funs arg tyname param_map ty
   else [])
;

value sig_item_top_funs arg tyname param_map ty =
  (if Ctxt.is_plugin_name arg "to_sexp" || Ctxt.is_plugin_name arg "sexp" then
    [To.sig_item_top_funs arg tyname param_map ty]
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.sig_item_top_funs arg tyname param_map ty
   else [])
;

value str_item_funs arg ((loc,_) as tyname) params ty =
  let param_map = List.mapi (fun i p ->
    match Pcaml.unvala (fst p) with [
      None -> failwith "cannot derive sexp-functions for type decl with unnamed type-vars"
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params in
  let l = str_item_top_funs arg tyname param_map ty in
  let types = sig_item_top_funs arg tyname param_map ty in
  List.map (fun (fname, body) ->
      let fty = List.assoc fname types in
      let fty = if param_map = [] then fty
        else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >> in
      (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [] >>)) l
;

value sig_item_funs arg ((loc,_) as tyname) params ty =
  let param_map = List.mapi (fun i p ->
    match Pcaml.unvala (fst p) with [
      None -> failwith "cannot derive sexp-functions for type decl with unnamed type-vars"
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params in
  let l = sig_item_top_funs arg tyname param_map ty in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value is_deriving_sexp attr = Pa_deriving.is_deriving "sexp" attr ;
value apply_deriving_sexp ctxt attr = Pa_deriving.apply_deriving "sexp" ctxt attr ;

value str_item_gen_sexp0 arg td =
  let arg = List.fold_left apply_deriving_sexp arg (Pcaml.unvala td.tdAttributes) in
  let tyname = Pcaml.unvala td.tdNam
  and params = Pcaml.unvala td.tdPrm
  and tk = td.tdDef in
  str_item_funs arg tyname params tk
;

value loc_of_type_decl td = fst (Pcaml.unvala td.tdNam) ;

value str_item_gen_sexp name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_sexp0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_sexp0 arg td =
  let tyname = Pcaml.unvala td.tdNam
  and params = Pcaml.unvala td.tdPrm
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
    List.iter (fun [ PvTag _ _ _ tyl _ -> List.iter brec (Pcaml.unvala tyl) | PvInh _ ty -> brec ty ])  branches
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
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_sexp" || id = "derive.to_sexp" ->
    let loc = loc_of_ctyp ty in
    let param_map = build_param_map ty in
    let e = To.fmt_to_top arg ~{msg=Printf.sprintf "%s.to_sexp"  (Ctxt.module_path_s arg)} param_map ty in
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
; alternates = ["to_sexp"; "of_sexp"]
; options = ["optional"; "strict"; "exn"]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>); ("strict", <:expr< False >>); ("exn", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"; "key"; "name"; "encoding"; "default"; "to_sexp"; "of_sexp"]
; expr_extensions = ["to_sexp"; "of_sexp"]
; expr = expr_sexp
; str_item = str_item_gen_sexp
; sig_item = sig_item_gen_sexp
})
;

