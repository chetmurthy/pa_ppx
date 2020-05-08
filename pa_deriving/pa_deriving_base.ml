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
value make msg loc params =
  List.mapi (fun i p ->
    match uv (fst p) with [
      None -> Ploc.raise loc (Failure (Printf.sprintf "cannot derive %s-functions for type decl with unnamed type-vars" msg))
    | Some na -> (na, Printf.sprintf "tp_%d" i)
    ]) params
;

value quantify_over_ctyp param_map fty =
  let loc = loc_of_ctyp fty in
  if param_map = [] then fty
  else <:ctyp< ! $list:(List.map fst param_map)$ . $fty$ >>
;
end
;

module PM = ParamMap ;
