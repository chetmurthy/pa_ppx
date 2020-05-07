(* camlp5r *)
(* pa_deriving_yojson.ml,v *)
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
                   when id = DC.allowed_attribute (DC.get arg) "yojson" attrna ->
                   e
                 | _ -> failwith "extract_allowed_attribute_expr" ]) attrs with [
    e -> Some e
  | exception Failure _ -> None ]
;

value make_param_map params =
  List.mapi (fun i p ->
    match uv (fst p) with [
      None -> failwith "cannot derive yojson-functions for type decl with unnamed type-vars"
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params
;

module To = struct

type attrmod_t = [ Nobuiltin ] ;

value to_yojson_fname arg tyname =
  if tyname = "t" then "to_yojson"
  else tyname^"_to_yojson"
;

value to_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = to_yojson_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> failwith "cannot derive yojson for type <<_>>"
| <:ctyp:< Yojson.Safe.t >> -> <:expr< fun x -> x >>
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

| <:ctyp:< Hashtbl.t >> ->
  <:expr< hashtbl_to_yojson >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "to_yojson" ->
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
    x -> x | exception Not_found -> failwith "pa_deriving.yojson: unrecognized param-var in type-decl"
  ] in
  <:expr< $lid:fmtf$ >>

| <:ctyp:< $lid:lid$ >> ->
  let fname = to_yojson_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = to_yojson_fname arg lid in
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
    (conspat, <:vala< None >>, <:expr< `List [ (`String $str:jscid$) ;  $body$ ] >>)

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

    (conspat, <:vala< None >>, <:expr< `List [ (`String $str:jscid$) :: $liste$ ] >>)

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

| [%unmatched_vala] -> failwith "pa_deriving_yojson.to_expression"
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

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = make_param_map (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let to_yojsonfname = to_yojson_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Yojson.Safe.t >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Yojson.Safe.t >> in
  (to_yojsonfname, toftype)
;

value sig_item_funs arg td =
  [sig_item_fun0 arg td]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> -> True | _ -> False ])) ;
  let (to_yojsonfname, toftype) = sig_item_fun0 arg td in
  let loc = fst (uv td.tdNam) in
  [ <:sig_item< value $lid:to_yojsonfname$ : $toftype$>> ]
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let param_map = make_param_map (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let to_yojsonfname = to_yojson_fname arg tyname in
  let to_e = fmt_to_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let to_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  let (_, fty) = sig_item_fun0 arg td in
  let fty = if param_map = [] then fty
        else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >> in
  [(<:patt< ( $lid:to_yojsonfname$ : $fty$ ) >>,
    Expr.abstract_over paramfun_patts
      <:expr< fun arg -> $to_e$ arg >>, <:vala< [] >>)]
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >> as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = make_param_map (uv td.tdPrm) in
    let ty = td.tdDef in
    let (to_yojsonfname, toftype) = sig_item_fun0 arg td in
    let sil = [<:sig_item< value $lid:to_yojsonfname$ : $toftype$>>] in
    let modname = Printf.sprintf "M_%s" to_yojsonfname in
    let field_type = if param_map = [] then toftype
      else <:ctyp< ! $list:(List.map fst param_map)$ . $toftype$ >> in
    [ <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:to_yojsonfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:to_yojsonfname$ ;
    end >> :: sil ]
| _ -> assert False
]
;

value extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >> as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = make_param_map (uv td.tdPrm) in
    let (to_yojsonfname, toftype) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" to_yojsonfname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving yojson] is missing when extending the type " to_yojsonfname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = if param_map = [] then toftype
      else <:ctyp< ! $list:(List.map fst param_map)$ . $toftype$ >> in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun (v, _) -> <:patt< ( type $lid:v$ ) >>) param_map) fexp in
    [ <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:to_yojsonfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< value $lid:to_yojsonfname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| _ -> assert False
]
;
end
;

module Of = struct

type attrmod_t = [ Nobuiltin ] ;

value of_yojson_fname arg tyname =
  if tyname = "t" then "of_yojson"
  else tyname^"_of_yojson"
;

value of_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = of_yojson_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< Yojson.Safe.t >> -> <:expr< fun x -> Result.Ok x >>
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

| <:ctyp:< Hashtbl.t >> ->
  <:expr< hashtbl_of_yojson >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "of_yojson" ->
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
    x -> x | exception Not_found -> failwith "pa_deriving.yojson: unrecognized param-var in type-decl"
  ] in
  <:expr< $lid:fmtf$ >>

| <:ctyp:< $lid:lid$ >> ->
  let fname = of_yojson_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = of_yojson_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record ~{cid=Some cid} loc arg (uv fields) in

    let conspat = <:patt< `List [ `String $str:jscid$ ; $recpat$ ] >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg "name" (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
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
  let lefts = List.map (mustLeft "of_yojson") lefts in
  let rights = List.map (mustRight "of_yojson") rights in

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

| [%unmatched_vala] -> failwith "pa_deriving_yojson.of_expression"
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

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = make_param_map (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let of_yojsonfname = of_yojson_fname arg tyname in
  let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Yojson.Safe.t -> Rresult.result $pty$ string >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Yojson.Safe.t -> Rresult.result $(Ctyp.applist ty paramtys)$ string >> in
  let e = (of_yojsonfname, offtype) in
  if not (Ctxt.is_exn arg) then (e, None) else
  let exn_name = of_yojsonfname^"_exn" in
  let exnftype = Ctyp.arrows_list loc argfmttys <:ctyp< Yojson.Safe.t -> $(Ctyp.applist ty paramtys)$ >> in
  let e' = (exn_name, exnftype) in
  (e, Some  e')
;

value gen_sig_items arg td =
  let loc = fst (uv td.tdNam) in
  let mk1sig (fname, fty) = <:sig_item< value $lid:fname$ : $fty$>> in
  match sig_item_fun0 arg td with [
    (f, None) -> [mk1sig f]
  | (f, Some g) -> [mk1sig f; mk1sig g] ]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> -> True | _ -> False ])) ;
  gen_sig_items arg td
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let param_map = make_param_map (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let of_yojsonfname = of_yojson_fname arg tyname in
  let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
  let paramfun_exprs = List.map (fun (_,ppf) -> <:expr< $lid:ppf$ >>) param_map in
  let body = fmt_of_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let (fun1, ofun2) = sig_item_fun0 arg td in
  let e = 
    let of_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $body$ >> in
    let (_, fty) = fun1 in
    let fty = if param_map = [] then fty
      else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >> in
    (<:patt< ( $lid:of_yojsonfname$ : $fty$ ) >>,
     Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $of_e$ arg >>, <:vala< [] >>) in
  if not (Ctxt.is_exn arg) then [e] else
    let exn_name = of_yojsonfname^"_exn" in
    let body = Expr.applist <:expr< $lid:of_yojsonfname$ >> paramfun_exprs in
    let body = <:expr< match $body$ arg with [ Rresult.Ok x -> x | Result.Error s -> failwith s ] >> in
    let of_e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $body$ >> in
    let (_, fty) = match ofun2 with [ None -> assert False | Some x -> x ] in
    let fty = if param_map = [] then fty
      else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >> in
    let e' = (<:patt< ( $lid:exn_name$ : $fty$ ) >>,
              Expr.abstract_over paramfun_patts
                <:expr< fun arg -> $of_e$ >>, <:vala< [] >>) in
    [e; e']
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >> as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = make_param_map (uv td.tdPrm) in
    let ty = td.tdDef in
    let sil = gen_sig_items arg td in
    let ((of_yojsonfname, offtype), _) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_yojsonfname in
    let field_type = if param_map = [] then offtype
      else <:ctyp< ! $list:(List.map fst param_map)$ . $offtype$ >> in
    [ <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:of_yojsonfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:of_yojsonfname$ ;
    end >> :: sil ]
| _ -> assert False
]
;

value extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >> as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = make_param_map (uv td.tdPrm) in
    let ((of_yojsonfname, offtype), oexn) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_yojsonfname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving yojson] is missing when extending the type " of_yojsonfname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = if param_map = [] then offtype
      else <:ctyp< ! $list:(List.map fst param_map)$ . $offtype$ >> in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun (v, _) -> <:patt< ( type $lid:v$ ) >>) param_map) fexp in
    [ <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:of_yojsonfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< value $lid:of_yojsonfname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| _ -> assert False
]
;

end
;

value str_item_funs arg td =
  (if Ctxt.is_plugin_name arg "to_yojson" || Ctxt.is_plugin_name arg "yojson" then
     To.str_item_funs arg td
   else []) @
  (if Ctxt.is_plugin_name arg "of_yojson" || Ctxt.is_plugin_name arg "yojson" then
     Of.str_item_funs arg td
   else [])
;

value sig_items arg td =
  (if Ctxt.is_plugin_name arg "to_yojson" || Ctxt.is_plugin_name arg "yojson" then
     To.sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_yojson" || Ctxt.is_plugin_name arg "yojson" then
     Of.sig_items arg td
   else [])
;

value extend_sig_items arg td =
  (if Ctxt.is_plugin_name arg "to_yojson" || Ctxt.is_plugin_name arg "yojson" then
     To.extend_sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_yojson" || Ctxt.is_plugin_name arg "yojson" then
     Of.extend_sig_items arg td
   else [])
;

value extend_str_items arg td =
  (if Ctxt.is_plugin_name arg "to_yojson" || Ctxt.is_plugin_name arg "yojson" then
     To.extend_str_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_yojson" || Ctxt.is_plugin_name arg "yojson" then
     Of.extend_str_items arg td
   else [])
;

value str_item_gen_yojson name arg = fun [
  <:str_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

|   <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (str_item_funs arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_yojson name arg = fun [
  <:sig_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >> as z ->
    let l = extend_sig_items arg z in
    <:sig_item< declare $list:l$ end >>

| <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (sig_items arg) tdl) in
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

value type_to_param_map t =
  let pvl = type_params t in
  List.mapi (fun i v -> (v, Printf.sprintf "tp_%d" i)) pvl
;

value expr_yojson arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_yojson" || id = "derive.to_yojson" ->
    let param_map = type_to_param_map ty in
    let e = To.fmt_to_top arg ~{msg=Printf.sprintf "%s.to_yojson"  (Ctxt.module_path_s arg)} param_map ty in
    let e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $e$ >> in
    let parampats = List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map in
    Expr.abstract_over parampats e

| <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_yojson" || id = "derive.of_yojson" ->
    let param_map = type_to_param_map ty in
    let e = Of.fmt_of_top ~{msg=Printf.sprintf "%s.of_yojson"  (Ctxt.module_path_s arg)} arg param_map ty in
    let e = <:expr< let open! Pa_ppx_runtime in let open! Stdlib in $e$ >> in
    let parampats = List.map (fun (_, f) -> <:patt< $lid:f$ >>) param_map in
    Expr.abstract_over parampats e
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "yojson"
; alternates = ["to_yojson"; "of_yojson"]
; options = ["optional"; "strict"; "exn"]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>); ("strict", <:expr< False >>); ("exn", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"; "key"; "name"; "encoding"; "default"; "to_yojson"; "of_yojson"]
; expr_extensions = ["to_yojson"; "of_yojson"]
; expr = expr_yojson
; str_item = str_item_gen_yojson
; sig_item = sig_item_gen_yojson
})
;

