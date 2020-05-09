(* camlp5r *)
(* pa_deriving_base.ml,v *)
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

module ParamMap = struct

type param_t =
  {
    type_id : string ;
    param_id : string
  }
;

value type_id p = p.type_id ;
value param_id p = p.param_id ;

value param_expr loc p = <:expr< $lid:param_id p$ >> ;
value param_patt loc p = <:patt< $lid:param_id p$ >> ;

value find id l =
  match List.find_opt (fun p -> type_id p = id) l with [
    None -> raise Not_found
  | Some p -> p ]
;

type t = list param_t ;

value make msg loc params =
  List.mapi (fun i p ->
    match uv (fst p) with [
      None -> Ploc.raise loc (Failure (Printf.sprintf "cannot derive %s-functions for type decl with unnamed type-vars" msg))
    | Some na -> { type_id = na ; param_id = Printf.sprintf "tp_%d" i }
    ]) params
;

value make_of_ids pvl =
  List.mapi (fun i v -> { type_id = v ; param_id = Printf.sprintf "tp_%d" i}) pvl
;

value quantify_over_ctyp param_map fty =
  let loc = loc_of_ctyp fty in
  if param_map = [] then fty
  else <:ctyp< ! $list:(List.map type_id param_map)$ . $fty$ >>
;
end
;

module PM = ParamMap ;

value wrap_type_constraints loc param_map funs types =
  List.map (fun (fname, body) ->
    let fty = List.assoc fname types in
    let fty = PM.quantify_over_ctyp param_map fty in
    let attrwarn39 = <:attribute_body< "ocaml.warning" "-39" ; >> in
    let attrwarn39 = <:vala< attrwarn39 >> in
    let attrwarn33 = <:attribute_body< "ocaml.warning" "-33" ; >> in
    let attrwarn33 = <:vala< attrwarn33 >> in
    (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [attrwarn39; attrwarn33] >>)) funs
;
