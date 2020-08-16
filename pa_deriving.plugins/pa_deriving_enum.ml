(* camlp5r *)
(* pa_deriving_enum.ml,v *)
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

value to_fname arg tyname =
  if tyname = "t" then "to_enum"
  else tyname^"_to_enum"
;

value of_fname arg tyname =
  if tyname = "t" then "of_enum"
  else tyname^"_of_enum"
;

value min_fname arg tyname =
  if tyname = "t" then "min"
  else "min_"^tyname
;

value max_fname arg tyname =
  if tyname = "t" then "max"
  else "max_"^tyname
;

value extract_value (attrs : MLast.attributes_no_anti) =
  let ex1 = fun [
    <:attribute_body< "value" $int:i$ ; >> -> Some (int_of_string i)
  | _ -> None
  ] in
  let rec exrec = fun [
    [] -> None
  | [h::t] -> match ex1 (uv h) with [ Some x -> Some x | None -> exrec t ]
  ] in
  exrec attrs
;

module PM = ParamMap(struct value arg_ctyp_f loc ty = assert False ; end) ;

value to_expression arg = fun [
  <:ctyp:< [ $list:l$ ] >> ->
  let (_, map,revacc) = List.fold_left (fun (idx, map, revacc) -> fun [
    <:constructor:< $uid:cid$ of $list:_$ $algattrs:attrs$ >> ->
    let idx = match extract_value attrs with [
      None -> idx
    | Some n -> n
    ] in
    let conspat = <:patt< $uid:cid$ >> in
    let consexpr = <:expr< $uid:cid$ >> in
    
    let body = <:expr< $int:(string_of_int idx)$ >> in

    (idx+1, [(consexpr,idx) :: map],
     [(conspat, <:vala< None >>, body) :: revacc])
  | gc -> Ploc.raise (loc_of_constructor gc) (Failure "pa_deriving_plugins.enum: unsupported constructor")
          ]) (0,[], []) l in
  let branches = List.rev revacc in
  (map, <:expr< fun [ $list:branches$ ] >>)

  | <:ctyp:< [= $list:l$ ] >> ->
  let (_, map,revacc) = List.fold_left (fun (idx,map,revacc) -> fun [
    PvTag loc cid _ _ attrs ->
    let idx = match extract_value (uv attrs) with [
      None -> idx | Some n -> n
    ] in
    let cid = uv cid in
    let conspat = <:patt< ` $cid$ >> in
    let consexpr = <:expr< ` $cid$ >> in
    
    let body = <:expr< $int:(string_of_int idx)$ >> in

    (idx+1, [(consexpr,idx) :: map],
     [(conspat, <:vala< None >>, body) :: revacc])


  | PvInh loc ty -> Ploc.raise loc (Failure "deriving.enum only works on variants sans inheritance")
  ]) (0, [], []) l in
  let branches = List.rev revacc in
  (map, <:expr< fun [ $list:branches$ ] >>)

| [%unmatched_vala] -> failwith "pa_deriving_enum.to_expression"
  ]
;

value of_expression arg map t =
  let loc = loc_of_ctyp t in
  let branches = List.map (fun (e,n) ->
    (<:patt< $int:(string_of_int n)$ >>, <:vala< None >>, <:expr< Some $e$ >>)) map in
  let branches = branches @ [ (<:patt< _ >>, <:vala< None >>, <:expr< None >>) ] in
  <:expr< fun [ $list:branches$ ] >>
;

value fmt_top arg t =
  let t = match t with [
    <:ctyp< $t1$ == $_priv:_$ $t2$ >> -> t2
  | t -> t
  ] in
  let (map, toexp) = to_expression arg t in
  let ofexp = of_expression arg map t in
  let vals = List.map snd map in
  let minval = List.fold_left min (List.hd vals) (List.tl vals) in
  let maxval = List.fold_left max (List.hd vals) (List.tl vals) in
  (toexp,ofexp,minval,maxval);

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let tofname = to_fname arg tyname in
  let offname = of_fname arg tyname in
  let minfname = min_fname arg tyname in
  let maxfname = max_fname arg tyname in
  let (toexp,ofexp,minval,maxval) = fmt_top arg ty in
  [(tofname, toexp);
   (offname, ofexp);
   (minfname, <:expr< $int:(string_of_int minval)$ >>);
   (maxfname, <:expr< $int:(string_of_int maxval)$ >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let tyname = uv tyname in
  let tofname = to_fname arg tyname in
  let offname = of_fname arg tyname in
  let minfname = min_fname arg tyname in
  let maxfname = max_fname arg tyname in
  let thety = <:ctyp< $lid:tyname$ >> in
  let toftype = <:ctyp< $thety$ -> Stdlib.Int.t >> in
  let offtype = <:ctyp< Stdlib.Int.t -> Stdlib.Option.t $thety$ >> in
  [(tofname, toftype);
   (offname, offtype);
   (minfname, <:ctyp< Stdlib.Int.t >>);
   (maxfname, <:ctyp< Stdlib.Int.t >>)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  PM.wrap_type_constraints loc [] funs types
;

value sig_item_funs arg td =
  let loc = loc_of_type_decl td in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_enum0 arg td =
  let params = uv td.tdPrm in
  if params <> [] then
    failwith "cannot derive enum-functions for type decl with type-vars"
  else
    str_item_funs arg td
;

value str_item_gen_enum name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_enum0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_enum0 arg td =
  let params = uv td.tdPrm in
  if params <> [] then
    Ploc.raise (loc_of_type_decl td) (Failure "cannot derive enum-functions for type decl with type-vars")
  else
    sig_item_funs arg td
;

value sig_item_gen_enum name arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_item_gen_enum0 arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "enum"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["value"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun _ _ -> assert False)
; ctyp = (fun _ _ -> assert False)
; str_item = str_item_gen_enum
; sig_item = sig_item_gen_enum
})
;

