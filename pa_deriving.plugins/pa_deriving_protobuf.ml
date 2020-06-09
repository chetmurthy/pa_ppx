(* camlp5r *)
(* pa_deriving_protobuf.ml,v *)
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
end ;

value tuplepatt loc l = if List.length l = 1 then List.hd l else <:patt< ( $list:l$ ) >> ;
value tupleexpr loc l = if List.length l = 1 then List.hd l else <:expr< ( $list:l$ ) >> ;

type encoding_t = [ Varint | Zigzag | Bits32 | Bits64 ] ;
type attrmod_t = {
  nobuiltin: bool
; encoding : option encoding_t
; key : option int
; message : bool
; arity : option [ = `List | `Array | `Optional ] } ;

value mt_attrmod = { nobuiltin = False ; encoding = None ; key = None ; message = False ; arity = None } ;
value attrmod_key a = match a.key with [ None -> 1 | Some n -> n ] ;
value attrmod_key_compare a b = Int.compare (attrmod_key a) (attrmod_key b) ;
value fmt_attrmod_key a = a |> attrmod_key |> string_of_int ;
value fmt_attrmod_modifier a =
 let loc = Ploc.dummy in
 match (a.message, a.arity) with [
  (False, None) -> <:expr< required >>
| (False, Some `Optional) -> <:expr< optional >>
| (_, Some `List) -> <:expr< list >>
| (_, Some `Array) -> <:expr< array >>
| (True, None) -> <:expr< required_last >>
| (True, Some `Optional) -> <:expr< optional_last >>
| _ -> assert False
] ;

module To = struct

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< $pty$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >> ; end) ;

value to_protobuf_fname arg tyname =
  tyname^"_to_protobuf"
;

value to_expression arg ?{coercion} ~{msg} param_map ty0 =
  let runtime_module =
    let loc = loc_of_ctyp ty0 in
    Base.expr_runtime_module <:expr< Runtime >> in
  let rec fmtrec ?{coercion} ?{attrmod={ (mt_attrmod) with message = True } } = fun [

(*
  <:ctyp:< $lid:lid$ >> when attrmod.nobuiltin ->
  let fname = to_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> failwith "cannot derive yojson for type <<_>>"
| <:ctyp:< Yojson.Safe.t >> -> <:expr< fun x -> x >>
| <:ctyp:< unit >> -> <:expr< $runtime_module$.Yojson.unit_to_yojson >>
*)
  <:ctyp:< int >> when attrmod.encoding = Some Zigzag ->
  (attrmod,
   <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int__zigzag ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits32 ->
  (attrmod,
   <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits64 ->
  (attrmod,
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int >> ->
  (attrmod,
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)
| <:ctyp:< bool >> ->
  (attrmod,
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (bool__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int32__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int32__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Varint ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int32__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int32__zigzag ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint32__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint32__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Varint ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint32__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint32__zigzag ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int64__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int64__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Varint ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int64__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (int64__zigzag ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint64__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint64__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Varint ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint64__varint ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (uint64__zigzag ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  (attrmod,
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (string__bytes ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| (<:ctyp:< bytes >> | <:ctyp:< Stdlib.Bytes.t >> | <:ctyp:< Bytes.t >>) ->
  (attrmod,
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (bytes__bytes ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

(*
| <:ctyp:< char >> -> <:expr< fun x -> $runtime_module$.Yojson.string_to_yojson (String.make 1 x) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< $runtime_module$.Yojson.nativeint_to_yojson >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> ->
    <:expr< fun x -> $runtime_module$.Yojson.string_to_yojson (Nativeint.to_string x) >>
*)
| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = None || attrmod.encoding = Some Bits64 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (float__bits64 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)

| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod,
 <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (float__bits32 ~{msg= $str:msg$ } ~{key= $int:fmt_attrmod_key attrmod$ }) >>)
(*
| <:ctyp:< Hashtbl.t >> ->
  <:expr< $runtime_module$.Yojson.hashtbl_to_yojson >>
*)
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "encoding" ->
    let encoding = match e with [
      <:expr< `varint >> -> Varint
    | <:expr< `zigzag >> -> Zigzag
    | <:expr< `bits32 >> -> Bits32
    | <:expr< `bits64 >> -> Bits64
    ] in
    fmtrec ~{attrmod={ (attrmod) with encoding = Some encoding } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "nobuiltin" ->
    fmtrec ~{attrmod={ (attrmod) with nobuiltin = True } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $int:key$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "key" ->
    let key = int_of_string key in
    fmtrec ~{attrmod={ (attrmod) with key = Some key } } t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } ty

| <:ctyp:< array $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } ty
(*
| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< $runtime_module$.Yojson.ref_to_yojson $fmt1$ >>
*)
| <:ctyp:< option $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } ty
(*
| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.yojson: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $lid:lid$ >> ->
  let fname = to_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = to_protobuf_fname arg lid in
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
*)
| <:ctyp:< ( $list:tyl$ ) >> ->
    let am_fmt_vars = List.mapi (fun i ty ->
      let attrmod = { (mt_attrmod) with key = Some (i+1) } in
      (fmtrec ~{attrmod=attrmod} ty, Printf.sprintf "v%d" i))
    tyl in
    (* ordered by occurrence in tuple-type *)
    let vars = List.map snd am_fmt_vars in
    (* ordered by key value *)
    let am_fmt_vars = List.sort (fun ((a, _),_) ((b, _),_) -> attrmod_key_compare a b) am_fmt_vars in
    let exps = List.map (fun ((_, fmt), v) -> <:expr< $fmt$ $lid:v$ encoder >>) am_fmt_vars in

    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let e = <:expr< fun ($list:varpats$) encoder -> do { $list:exps$ } >> in
    let e = if not attrmod.message then
      <:expr< fun v encoder -> do {
                Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder;
                Protobuf.Encoder.nested ($e$ v) encoder } >>
      else e in
  (attrmod, e)

| <:ctyp:< { $list:fields$ } >> -> do {
(* (1) convert fields into a tuple-type
   (2) generate encoder
   (3) convert record to tuple
   (4) and apply encoder
   N.B. since this is a record, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let tupty =
    let l = List.map (fun (_, _, _, ty, attrs) ->
      let keyattrs = List.find_all (DC.is_allowed_attribute (DC.get arg) "protobuf" "key") (uv attrs) in do {
        if List.length keyattrs <> 1 then
          Ploc.raise loc (Failure "MUST specify exactly one @key for each field")
        else () ;
        <:ctyp< $ty$ [@ $_attribute:List.hd keyattrs$ ] >>
      }) fields in
    <:ctyp< ( $list:l$ ) >> in
  let (_, e) = fmtrec ~{attrmod=attrmod} tupty in
  let fields_as_tuple =
    let l = List.map (fun (_, n, _, _, _) ->
      <:expr< v . $lid:n$ >>
    ) fields in
    tupleexpr loc l in
  (attrmod, <:expr< fun v encoder -> $e$ $fields_as_tuple$ encoder >>)

  }

| [%unmatched_vala] -> failwith "pa_deriving_protobuf.to_expression"
]
(*
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
*)
in fmtrec ?{coercion=coercion} ty0
;


value fmt_to_top arg ~{coercion} ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  snd (to_expression arg ~{coercion=coercion} ~{msg=msg} params t2)
| t -> snd (to_expression arg ~{coercion=coercion} ~{msg} params t)
]
;

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let to_protobuffname = to_protobuf_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >> in
  (to_protobuffname, toftype)
;

value sig_item_funs arg td =
  [sig_item_fun0 arg td]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (to_protobuffname, toftype) = sig_item_fun0 arg td in
  let loc = loc_of_type_decl td in
  [ <:sig_item< value $lid:to_protobuffname$ : $toftype$>> ]
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tk = td.tdDef in
  let tyname = uv tyname in
  let ty =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    (Ctyp.applist ty paramtys) in
  let coercion =
    monomorphize_ctyp ty in
  let to_protobuffname = to_protobuf_fname arg tyname in
  let to_e = fmt_to_top arg ~{coercion=coercion} ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map tk in
  let to_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let (_, fty) = sig_item_fun0 arg td in
  let fty = PM.quantify_over_ctyp param_map fty in
  let argexp =
    if uv td.tdPrv && is_type_abbreviation tk then
      <:expr< ( arg : $monomorphize_ctyp ty$ :> $monomorphize_ctyp tk$ ) >>
    else <:expr< arg >> in
  [(<:patt< ( $lid:to_protobuffname$ : $fty$ ) >>,
    Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg encoder -> $to_e$ $argexp$ encoder >>, <:vala< [] >>)]
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let (to_protobuffname, toftype) = sig_item_fun0 arg td in
    let sil = [<:sig_item< value $lid:to_protobuffname$ : $toftype$>>] in
    let modname = Printf.sprintf "M_%s" to_protobuffname in
    let field_type = PM.quantify_over_ctyp param_map toftype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:to_protobuffname$ = { f: mutable  $field_type$ } ;
      value f : $lid:to_protobuffname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let (to_protobuffname, toftype) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" to_protobuffname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving protobuf] is missing when extending the type " to_protobuffname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map toftype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:to_protobuffname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:to_protobuffname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (to_protobuf_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "protobuf" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = snd(to_expression arg ~{msg=String.escaped (Pp_MLast.show_longid_lident t)} param_map ty) in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps@[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

module Of = struct

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< Pa_ppx_protobuf.Runtime.Protobuf.Decoder.t -> $pty$ >> ; end) ;

value of_protobuf_fname arg tyname =
  tyname^"_from_protobuf"
;

(**
EXAMPLE (type i1 = int [@key 1])
let i1_from_protobuf_manually decoder =
  let v0 = None in
  let update_v0 v0 (newv : int) = Some newv in
  let rec derec v0 = match Protobuf.Decoder.key decoder with
    | Some (1, kind) ->
      let v0 = update_v0 v0 
      (let open Pa_ppx_protobuf.Runtime.Decode in
            ((decode0 int__varint ~msg:"Test_syntax.i1" kind) decoder))
       in
      derec v0
    | Some (_, kind) -> ( Protobuf.Decoder.skip decoder kind ; derec v0 )
    | None -> v0
  in match derec v0 with
    Some v0 -> v0
  | _ -> raise
        (let open Protobuf.Decoder in
            Failure (Missing_field "Test_syntax.i1"))

GENERALIZE
let i1_from_protobuf_manually = (*e1*) fun decoder ->
  (*e2*)<initialize vars v_i for each type with either None or the empty value of the type (for optional, list, array)>
  (*e3*)<define updaters for each type>
  (*e4*)let rec derec (<tuple-of-vars>) = match Protobuf.Decoder.key decoder with
    (*branch5*)| Some (<key_i>, kind) ->
      (*e5*)let v_i = update_v_i v_i
      (let open Pa_ppx_protobuf.Runtime.Decode in
            ((<decoder-expression for type> ~msg:"Test_syntax.i1" kind) decoder))
       in
      derec <tuple-of-vars>
    | Some (_, kind) -> ( Protobuf.Decoder.skip decoder kind ; derec <tuple-of-vars> )
    | None -> <tuple-of-vars>
  in (*e6*)match derec <tuple-of-vars> with
    <final-value-checks-projections-for-each-type>  -> <final-value-for-each-type>
  | _ -> raise
        (let open Protobuf.Decoder in
            Failure (Missing_field "Test_syntax.i1"))
*)
value demarshal_to_tuple loc arg ~{msg} am_kind_fmt_vars =
  let initvals = List.map (fun (am, kind, fmt, v) ->
    let e = match am.arity with [
      None | Some `Optional -> <:expr< None >>
    | Some (`List | `Array)  -> <:expr< [] >>
    ] in
    (<:patt< $lid:v$ >>, e, <:vala< [] >>)) am_kind_fmt_vars in
  let updaters = List.map (fun (am, _, _, v) ->
    let fname = Printf.sprintf "update_%s" v in
    let e = match am.arity with [
      None -> <:expr< Some newv >>
    | Some `Optional -> <:expr< Some newv >>
    | Some ( `List | `Array ) -> <:expr< [ newv :: $lid:v$ ] >>
    ] in
    (<:patt< $lid:fname$ >>,
     <:expr< fun $lid:v$ newv -> $e$ >>, 
     <:vala< [] >>)) am_kind_fmt_vars in
  let tuple_of_vars_patt =
    let l = List.map (fun (_, _, _, v) -> <:patt< $lid:v$ >>) am_kind_fmt_vars in
    tuplepatt loc l in
  let tuple_of_vars_expr =
    let l = List.map (fun (_, _, _, v) -> <:expr< $lid:v$ >>) am_kind_fmt_vars in
    tupleexpr loc l in
  let final_value_patts = List.map (fun (am, _, _, v) ->
    match am.arity with [
      None -> <:patt< Some $lid:v$ >>
    | Some ( `List | `Array | `Optional ) -> <:patt< $lid:v$ >>
    ]) am_kind_fmt_vars in
  let final_value_patt = tuplepatt loc final_value_patts in
  let final_value_exprs = List.map (fun (am, _, _, v) ->
    match am.arity with [
      None -> <:expr< $lid:v$ >>
    | Some `List -> <:expr< List.rev $lid:v$ >>
    | Some `Array -> <:expr< Array.of_list (List.rev $lid:v$) >>
    | Some `Optional -> <:expr< $lid:v$ >>
    ]) am_kind_fmt_vars in
  let final_value_expr = tupleexpr loc final_value_exprs in

  let e6 = <:expr< match derec $tuple_of_vars_expr$ with [
     $final_value_patt$ -> $final_value_expr$
   | _ -> raise (let open Protobuf.Decoder in Failure (Missing_field $str:msg$)) ] >> in

  let branch5s = List.map (fun (am, kind, fmt, v) ->
    let updatename = Printf.sprintf "update_%s" v in
    let e5 =
      <:expr< let $lid:v$ = $lid:updatename$ $lid:v$
       (let open Pa_ppx_protobuf.Runtime.Decode in
         $fmt$ ~{msg= $str:msg$ } decoder)
         in derec $tuple_of_vars_expr$ >> in
    let missing = <:expr< raise (let open Protobuf.Decoder in Failure (Missing_field $str:msg$)) >> in
      [(<:patt< Some ($int:fmt_attrmod_key am$, $kind$) >>, <:vala< None >>, e5) ;
       (<:patt< Some ($int:fmt_attrmod_key am$, _) >>, <:vala< None >>, missing)]
  ) am_kind_fmt_vars in
  let branch5s = List.concat branch5s in

  let fallback_branches = [
    (<:patt< Some (_, kind) >>, <:vala< None >>,
     <:expr< do { Protobuf.Decoder.skip decoder kind ; derec $tuple_of_vars_expr$ } >>) ;
    (<:patt< None >>, <:vala< None >>, tuple_of_vars_expr)
  ] in

  let e4 = <:expr< let rec derec $tuple_of_vars_patt$ = match Protobuf.Decoder.key decoder with
                   [ $list:branch5s@fallback_branches$ ]
                   in $e6$ >> in

  let e3 = <:expr< let $list:updaters$ in $e4$ >> in
  let e2 = <:expr< let $list:initvals$ in $e3$ >> in
  <:expr< fun ~{msg} decoder -> $e2$ >>
;

value of_expression arg ~{attrmod} ~{msg} param_map ty0 =
  let runtime_module =
    let loc = loc_of_ctyp ty0 in
    Base.expr_runtime_module <:expr< Runtime >> in
  let rec fmtrec ?{attrmod=mt_attrmod} = fun [
(*
  <:ctyp:< $lid:lid$ >> when attrmod.nobuiltin ->
  let fname = of_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< Protobuf.Safe.t >> -> <:expr< fun x -> Result.Ok x >>
| <:ctyp:< unit >> -> <:expr< $runtime_module$.Protobuf.unit_of_protobuf $str:msg$ >>
*)
  <:ctyp:< int >> when attrmod.encoding = Some Zigzag -> 
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 int__zigzag) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits32 -> 
  (attrmod, <:patt< Protobuf.Bits32 >>,
  <:expr< (decode0 int__bits32) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits64 -> 
  (attrmod, <:patt< Protobuf.Bits64 >>,
  <:expr< (decode0 int__bits64) >>)

| <:ctyp:< int >> -> 
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 int__varint)>>)

| <:ctyp:< bool >> ->
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 bool__variant) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 int32__bits32) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 int32__bits64) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int32__varint) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int32__zigzag) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 uint32__bits32) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 uint32__bits64) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint32__varint) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint32__zigzag) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 int64__bits64) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 int64__bits32) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int64__varint) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int64__zigzag) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 uint64__bits64) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 uint64__bits32) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint64__varint) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint64__zigzag) >>)

| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  (attrmod, <:patt< Protobuf.Bytes >>,
 <:expr< (decode0 string__bytes) >>)

| (<:ctyp:< bytes >> | <:ctyp:< Stdlib.Bytes.t >> | <:ctyp:< Bytes.t >>) ->
  (attrmod, <:patt< Protobuf.Bytes >>,
 <:expr< (decode0 bytes__bytes) >>)
(*
| <:ctyp:< char >> -> <:expr< fun [ `String x ->
          if (String.length x) = 1
          then Result.Ok (x.[0])
          else Result.Error $str:msg$
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< $runtime_module$.Protobuf.nativeint_of_protobuf $str:msg$ >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> -> <:expr< fun [
        `String x -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
*)
| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = None || attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 float__bits64) >>)

| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 float__bits32) >>)
(*
| <:ctyp:< Hashtbl.t >> ->
  <:expr< $runtime_module$.Protobuf.hashtbl_of_protobuf >>
*)
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "encoding" ->
    let encoding = match e with [
      <:expr< `varint >> -> Varint
    | <:expr< `zigzag >> -> Zigzag
    | <:expr< `bits32 >> -> Bits32
    | <:expr< `bits64 >> -> Bits64
    ] in
    fmtrec ~{attrmod= { (attrmod) with encoding = Some encoding } } t
(*
| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "nobuiltin" ->
    fmtrec ~{attrmod={ (attrmod) with nobuiltin = True } } t
*)
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $int:key$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "key" ->
    let key = int_of_string key in
    fmtrec ~{attrmod={ (attrmod) with key = Some key } } t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } ty

| <:ctyp:< array $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } ty
(*
| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< $runtime_module$.Protobuf.ref_of_protobuf $fmt1$ >>
*)
| <:ctyp:< option $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } ty
(*
| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.protobuf: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $lid:lid$ >> ->
  let fname = of_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = of_protobuf_fname arg lid in
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
  let ty0 = monomorphize_ctyp ty0 in
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
    let consexp = <:expr< Result.Ok ( $consexp$ :> $ty0$ ) >> in
    let unmarshe = List.fold_right2 (fun fmte v rhs ->
        <:expr< Rresult.R.bind ($fmte$ $lid:v$) (fun $lid:v$ -> $rhs$) >>) fmts vars consexp in

    Left (conspat, <:vala< None >>, unmarshe)
  }

  | PvInh _ ty ->
    let fmtf = fmtrec ty in
    Right fmtf
  ]) l in
  let (lefts,rights) = filter_split isLeft branches in
  let lefts = List.map (mustLeft "of_protobuf") lefts in
  let rights = List.map (mustRight "of_protobuf") rights in

  let righte = List.fold_right (fun fmtf rhs ->
    <:expr< match $fmtf$ json with [
                     Result.Ok result -> Result.Ok (result :> $ty0$)
                   | Result.Error _ -> $rhs$ ] >>)
    rights <:expr< Result.Error $str:msg$ >> in

  let last_branch = (<:patt< json >>, <:vala< None >>,
                     righte) in

  let branches = lefts @ [ last_branch ] in
  <:expr< fun [ $list:branches$ ] >>
*)
| <:ctyp:< ( $list:tyl$ ) >> ->
    let am_kind_fmt_vars = List.mapi (fun i ty ->
      let attrmod = { (mt_attrmod) with key = Some (i+1) } in
      let (am, kind, fmt) = fmtrec ~{attrmod=attrmod} ty in
      (am, kind, fmt, Printf.sprintf "v_%d" i)) tyl in
    let e = demarshal_to_tuple loc arg ~{msg=msg} am_kind_fmt_vars in
    let e = if not attrmod.message then
      <:expr< fun ~{msg} decoder -> $e$ ~{msg=msg} (Protobuf.Decoder.nested decoder) >>
    else e in
    (attrmod, <:patt< Protobuf.Bytes >>, e)

| <:ctyp:< { $list:fields$ } >> -> do {
(* (1) convert fields into a tuple-type
   (2) generate decoder
   (3) apply encoder
   (4) and convert tuple to record
   N.B. since this is a record, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let tupty =
    let l = List.map (fun (_, _, _, ty, attrs) ->
      let keyattrs = List.find_all (DC.is_allowed_attribute (DC.get arg) "protobuf" "key") (uv attrs) in do {
        if List.length keyattrs <> 1 then
          Ploc.raise loc (Failure "MUST specify exactly one @key for each field")
        else () ;
        <:ctyp< $ty$ [@ $_attribute:List.hd keyattrs$ ] >>
      }) fields in
    <:ctyp< ( $list:l$ ) >> in
  let (_, _, e) = fmtrec ~{attrmod=attrmod} tupty in
  let fields_as_tuple_patt =
    let l = List.map (fun (_, n, _, _, _) ->
      <:patt< $lid:n$ >>
    ) fields in
    tuplepatt loc l in
  let recexpr =
    let l = List.map (fun (_, n, _, _, _) ->
      (<:patt< $lid:n$ >>, <:expr< $lid:n$ >>)
    ) fields in
    <:expr< { $list:l$ } >> in
  (attrmod, <:patt< Protobuf.Bytes >>,
   <:expr< fun ~{msg} decoder -> match $e$ msg decoder with
      [ $fields_as_tuple_patt$ -> $recexpr$ ] >>)
  }


| [%unmatched_vala] -> failwith "pa_deriving_protobuf.of_expression"
]
(*
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
*)
  in
  let (am, kind, fmt) = fmtrec ~{attrmod=attrmod} ty0 in
  let loc = loc_of_ctyp ty0 in
  match ty0 with [
    <:ctyp< ( $list:_$ ) >> -> (am, fmt)
  | _ ->
    let e = demarshal_to_tuple loc arg ~{msg=msg} [(am, kind, fmt, "v")] in
    (attrmod, e)
  ]
;

value fmt_of_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  snd (of_expression arg ~{attrmod= { (mt_attrmod) with message = True } } ~{msg=msg} params t2)
| t -> snd (of_expression arg ~{attrmod= { (mt_attrmod) with message = True } } ~{msg=msg} params t)
]
;

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let of_protobuffname = of_protobuf_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< '$PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Protobuf.Decoder.t -> $pty$ >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Protobuf.Decoder.t ->  $(Ctyp.applist ty paramtys)$ >> in
  let e = (of_protobuffname, offtype) in
   (e, None)
;

value gen_sig_items arg td =
  let loc = loc_of_type_decl td in
  let mk1sig (fname, fty) = <:sig_item< value $lid:fname$ : $fty$>> in
  match sig_item_fun0 arg td with [
    (f, None) -> [mk1sig f]
  | (f, Some g) -> [mk1sig f; mk1sig g] ]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  gen_sig_items arg td
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let of_protobuffname = of_protobuf_fname arg tyname in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let paramfun_exprs = List.map (PM.arg_expr loc) param_map in
  let msg = Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname in
  let body = fmt_of_top arg ~{msg=msg} param_map ty in
  let (fun1, ofun2) = sig_item_fun0 arg td in
  let e = 
    let of_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $body$ >> in
    let (_, fty) = fun1 in
    let fty = PM.quantify_over_ctyp param_map fty in
    (<:patt< ( $lid:of_protobuffname$ : $fty$ ) >>,
     Expr.abstract_over (paramtype_patts@paramfun_patts)
       <:expr< fun arg -> $of_e$ $str:msg$ arg >>, <:vala< [] >>) in
   [e]
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let sil = gen_sig_items arg td in
    let ((of_protobuffname, offtype), _) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_protobuffname in
    let field_type = PM.quantify_over_ctyp param_map offtype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:of_protobuffname$ = { f: mutable  $field_type$ } ;
      value f : $lid:of_protobuffname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
   as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let ((of_protobuffname, offtype), oexn) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_protobuffname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving protobuf] is missing when extending the type " of_protobuffname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map offtype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:of_protobuffname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:of_protobuffname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (of_protobuf_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "protobuf" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = snd (of_expression arg ~{attrmod=mt_attrmod} ~{msg=String.escaped (Pp_MLast.show_longid_lident t)} param_map ty) in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    (* remove the catch-branch *)
    let (_, branches) = sep_last branches in 
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps @[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

value str_item_funs arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.str_item_funs arg td
   else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.str_item_funs arg td
   else [])
;

value sig_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.sig_items arg td
   else [])
;

value extend_sig_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.extend_sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.extend_sig_items arg td
   else [])
;

value extend_str_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.extend_str_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.extend_str_items arg td
   else [])
;

value str_item_gen_protobuf name arg = fun [
  <:str_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< exception $excon:_$ $itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (str_item_funs arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_protobuf name arg = fun [
  <:sig_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_sig_items arg z in
    <:sig_item< declare $list:l$ end >>

| <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    <:sig_item< declare $list:[]$ end >>

| <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>

| _ -> assert False ]
;

value expr_protobuf arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_protobuf" || id = "derive.to_protobuf" ->
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let coercion = monomorphize_ctyp ty in
    let e = To.fmt_to_top arg ~{coercion=coercion} ~{msg=Printf.sprintf "%s.to_protobuf"  (Ctxt.module_path_s arg)} param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (To.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $To.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e

| <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_protobuf" || id = "derive.of_protobuf" ->
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let e = Of.fmt_of_top ~{msg=Printf.sprintf "%s.of_protobuf"  (Ctxt.module_path_s arg)} arg param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (Of.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $Of.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e
| _ -> assert False ]
;

value ctyp_protobuf arg = fun [
  <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_protobuf" || id = "derive.to_protobuf" ->
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let argfmttys = List.map (To.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< $ty$ -> Protobuf.Safe.t >>

| <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_protobuf" || id = "derive.of_protobuf" ->
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let argfmttys = List.map (Of.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< Protobuf.Safe.t -> Rresult.result $ty$ string >>

| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "protobuf"
; alternates = []
; options = ["optional" ; "protoc" ; "protoc_import" ]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>) ]
; alg_attributes = ["key"; "default"; "encoding"; "bare"; "packed"; "nobuiltin"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun _ _ -> assert False)
; ctyp = (fun _ _ -> assert False)
; str_item = str_item_gen_protobuf
; sig_item = sig_item_gen_protobuf
})
;


