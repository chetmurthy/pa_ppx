(* camlp5r *)
(* pa_deriving_yojson.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;

module Ctxt = struct
  include Pa_passthru.Ctxt ;

end ;

value to_yojson_fname arg tyname =
  if tyname = "t" then "to_yojson"
  else tyname^"_to_yojson"
;

value of_yojson_fname arg tyname =
  if tyname = "t" then "of_yojson"
  else tyname^"_of_yojson"
;

type attrmod_t = [ Nobuiltin ] ;

value to_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = to_yojson_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> <:expr< let open Fmt in (const string "_") >>
| <:ctyp:< unit >> -> <:expr< fun () -> `Null >>
| <:ctyp:< int >> -> <:expr< fun x -> `Int x >>
| <:ctyp:< bool >> -> <:expr<  Fmt.bool >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< fun x -> `Intlit (Int32.to_string x) >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< fun x -> `Intlit (Int64.to_string x) >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%S" arg) >>
| <:ctyp:< bytes >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%S" (Bytes.to_string arg)) >>
| <:ctyp:< char >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%C" arg) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< fun x -> `Intlit (Nativeint.to_string x) >>
| <:ctyp:< float >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%F" arg) >>

| <:ctyp:< $t$ [@ $attrid:id$ ] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

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
| [%unmatched_vala] -> failwith "pa_deriving_yojson.fmt_expression"
]
in fmtrec ty0
;

value of_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = of_yojson_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> <:expr< let open Fmt in (const string "_") >>
| <:ctyp:< unit >> -> <:expr< fun [ `Null -> Result.Ok () | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int >> -> <:expr< fun [`Int x -> Result.Ok x | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< bool >> -> <:expr<  Fmt.bool >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< fun [
        `Int x -> Result.Ok (Int32.of_int x)
      | `Intlit x -> Result.Ok (Int32.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< fun [
      `Int x -> Result.Ok (Int64.of_int x)
      | `Intlit x -> Result.Ok (Int64.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%S" arg) >>
| <:ctyp:< bytes >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%S" (Bytes.to_string arg)) >>
| <:ctyp:< char >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%C" arg) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< fun [
        `Int x -> Result.Ok (Nativeint.of_int x)
      | `Intlit x -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< float >> -> <:expr< fun ofmt arg -> let open Fmt in (pf ofmt "%F" arg) >>

| <:ctyp:< $t$ [@ $attrid:id$ ] >> when id = DC.allowed_attribute (DC.get arg) "yojson" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t


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

| [%unmatched_vala] -> failwith "pa_deriving_yojson.fmt_expression"
]
in fmtrec ty0
;

value fmt_to_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  to_expression arg ~{msg=msg} params t2
| t -> to_expression arg ~{msg} params t
]
;

value fmt_of_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  of_expression arg ~{msg=msg} params t2
| t -> of_expression arg ~{msg=msg} params t
]
;

value str_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in

  let (to_fname, to_exp) =
    let to_yojsonfname = to_yojson_fname arg tyname in
    let to_e = fmt_to_top arg ~{msg=Printf.sprintf "to_yojson.%s" tyname} param_map ty in
    let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
    (to_yojsonfname, Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $to_e$ arg >>) in

  let (of_fname, of_exp) =
    let of_yojsonfname = of_yojson_fname arg tyname in
    let of_e = fmt_of_top arg ~{msg=Printf.sprintf "of_yojson.%s" tyname} param_map ty in
    let paramfun_patts = List.map (fun (_,ppf) -> <:patt< $lid:ppf$ >>) param_map in
    (of_yojsonfname, Expr.abstract_over paramfun_patts
       <:expr< fun arg -> $of_e$ arg >>) in
  [(to_fname, to_exp); (of_fname, of_exp)]
;

value sig_item_top_funs arg (loc, tyname) param_map ty =
  let tyname = Pcaml.unvala tyname in

  let (to_fname, to_type) =
    let to_yojsonfname = to_yojson_fname arg tyname in
    let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
    let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Yojson.Safe.t >>) paramtys in  
    let ty = <:ctyp< $lid:tyname$ >> in
    let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Yojson.Safe.t >> in
    (to_yojsonfname, toftype) in

  let (of_fname, of_type) =
    let of_yojsonfname = of_yojson_fname arg tyname in
    let paramtys = List.map (fun (tyna, _) -> <:ctyp< '$tyna$ >>) param_map in
    let argfmttys = List.map (fun pty -> <:ctyp< Yojson.Safe.t -> Rresult.result $pty$ string >>) paramtys in  
    let ty = <:ctyp< $lid:tyname$ >> in
    let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Yojson.Safe.t -> Rresult.result $(Ctyp.applist ty paramtys)$ string >> in
    (of_yojsonfname, offtype) in
    [(of_fname, of_type); (to_fname, to_type)]
;

value str_item_funs arg ((loc,_) as tyname) params ty =
  let param_map = List.mapi (fun i p ->
    match Pcaml.unvala (fst p) with [
      None -> failwith "cannot derive yojson-functions for type decl with unnamed type-vars"
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
      None -> failwith "cannot derive yojson-functions for type decl with unnamed type-vars"
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params in
  let l = sig_item_top_funs arg tyname param_map ty in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value is_deriving_yojson attr = Pa_deriving.is_deriving "yojson" attr ;
value apply_deriving_yojson ctxt attr = Pa_deriving.apply_deriving "yojson" ctxt attr ;

value str_item_gen_yojson0 arg td =
  let arg = List.fold_left apply_deriving_yojson arg (Pcaml.unvala td.tdAttributes) in
  let tyname = Pcaml.unvala td.tdNam
  and params = Pcaml.unvala td.tdPrm
  and tk = td.tdDef in
  str_item_funs arg tyname params tk
;

value loc_of_type_decl td = fst (Pcaml.unvala td.tdNam) ;

value str_item_gen_yojson arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_yojson0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_yojson0 arg td =
  let tyname = Pcaml.unvala td.tdNam
  and params = Pcaml.unvala td.tdPrm
  and tk = td.tdDef in
  sig_item_funs arg tyname params tk
;

value sig_item_gen_yojson arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_item_gen_yojson0 arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

value expr_yojson arg = fun [
  <:expr:< [% $attrid:id$: $type:ty$ ] >> when id = "to_yojson" || id = "derive.to_yojson" ->
    let loc = loc_of_ctyp ty in
    let e = fmt_to_top arg ~{msg="to_yojson"} [] ty in
    <:expr< fun arg -> Format.asprintf "%a" $e$ arg >>
| <:expr:< [% $attrid:id$: $type:ty$ ] >> when id = "of_yojson" || id = "derive.of_yojson" ->
    let loc = loc_of_ctyp ty in
    let e = fmt_of_top ~{msg="of_yojson"} arg [] ty in
    <:expr< fun arg -> Format.asprintf "%a" $e$ arg >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "yojson"
; alternates = ["to_yojson"; "of_yojson"]
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"; "key"; "name"; "encodin"; "default"]
; expr_extensions = ["to_yojson"; "of_yojson"]
; expr = expr_yojson
; str_item = str_item_gen_yojson
; sig_item = sig_item_gen_yojson
})
;

