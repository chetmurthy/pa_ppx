(* camlp5r *)
(* pa_undo_deriving.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value flatten_declare_str_item l =
  l |> List.map (fun [
      StDcl _ l -> uv l
    | x -> [x] ])
    |> List.concat
;

value flatten_declare_sig_item l =
  l |> List.map (fun [
      SgDcl _ l -> uv l
    | x -> [x] ])
    |> List.concat
;

value is_deriving_inline_attribute (attr : attribute) = attr_id attr = "deriving_inline" ;

value undo_deriving_inline tdl =
  let (last, tdl) = sep_last tdl in
  let (deriving_attr, other_attrs) =
    match filter_split is_deriving_inline_attribute (uv last.tdAttributes) with [
      (([] | [_ ; _ :: _]), _) -> failwith "should only be one @@deriving_inline attribute"
    | ([a], others) -> (a, others)
    ] in
  let (loc_attrid, payload) = uv deriving_attr in
  let idloc = fst (uv loc_attrid) in
  let newattr = (<:vala< (idloc, "deriving") >>, payload) in
  let attrs = other_attrs @ [ <:vala< newattr >> ] in
  let last = { (last) with tdAttributes = <:vala< attrs >> } in
  tdl @ [ last ]
;

value refold_deriving_inline_structure arg l =
  let rec rerec acc = fun [
    [] -> List.rev acc
  | [ (<:str_item:< type $flag:nrfl$ $list:tdl$ >>) :: t ] 
      when 1 = count is_deriving_inline_attribute (uv (fst (sep_last tdl)).tdAttributes) ->
      let tdl = undo_deriving_inline tdl in
      consrec [ <:str_item:< type $flag:nrfl$ $list:tdl$ >> :: acc ] t

  | [ h :: t ] -> rerec [h :: acc] t
  ]
  and consrec acc = fun [
    [] -> failwith "refold_deriving_inline: unmatched @@deriving_inline and @@@end"
  | [ <:str_item< [@@@"end"] >> :: t ] -> rerec acc t
  | [ _ :: t ] -> consrec acc t
  ] in
  rerec [] l
;

value refold_deriving_inline_signature arg l =
  let rec rerec acc = fun [
    [] -> List.rev acc
  | [ (<:sig_item:< type $flag:nrfl$ $list:tdl$ >>) :: t ] 
      when 1 = count is_deriving_inline_attribute (uv (fst (sep_last tdl)).tdAttributes) ->
      let tdl = undo_deriving_inline tdl in
      consrec [ <:sig_item:< type $flag:nrfl$ $list:tdl$ >> :: acc ] t

  | [ h :: t ] -> rerec [h :: acc] t
  ]
  and consrec acc = fun [
    [] -> failwith "refold_deriving_inline: unmatched @@deriving_inline and @@@end"
  | [ <:sig_item< [@@@"end"] >> :: t ] -> rerec acc t
  | [ _ :: t ] -> consrec acc t
  ] in
  rerec [] l
;

value rec structure_has_end_attribute l =
  List.exists (fun [
    <:str_item< [@@@"end"] >> -> True
  | <:str_item< declare $list:l$ end >> -> structure_has_end_attribute l
  | _ -> False ]) l ;
value rec signature_has_end_attribute l =
  List.exists (fun [
    <:sig_item< [@@@"end"] >> -> True
  | <:sig_item< declare $list:l$ end >> -> signature_has_end_attribute l
  | _ -> False ]) l ;

value implem_has_end_attribute (l,_) =
  List.exists (fun [
    (<:str_item< [@@@"end"] >>,_) -> True
  | (<:str_item< declare $list:l$ end >>, _) -> structure_has_end_attribute l
  | _ -> False ]) l ;
value interf_has_end_attribute (l,_) =
  List.exists (fun [
    (<:sig_item< [@@@"end"] >>,_) -> True
  | (<:sig_item< declare $list:l$ end >>, _) -> signature_has_end_attribute l
  | _ -> False ]) l ;

value registered_implem arg z =
  if not (implem_has_end_attribute z) then z else
  let (l, st) = z in
  let l = refold_deriving_inline_structure arg (List.map fst l) in
  (List.map (fun si -> (si, loc_of_str_item si)) l, st)
;

value registered_interf arg z =
  if not (interf_has_end_attribute z) then z else
  let (l, st) = z in
  let l = refold_deriving_inline_signature arg (List.map fst l) in
  (List.map (fun si -> (si, loc_of_sig_item si)) l, st)
;

value registered_structure arg z =
  if not (structure_has_end_attribute z) then z else
    refold_deriving_inline_structure arg z ;
value registered_signature arg z =
  if not (signature_has_end_attribute z) then z else
    refold_deriving_inline_signature arg z ;

value install () =
let ef = EF.mk() in
let ef = EF.{ (ef) with
  implem = extfun ef.implem with [
    z ->
      fun arg fallback ->
        Some (registered_implem arg z)
  ] } in

let ef = EF.{ (ef) with
  interf = extfun ef.interf with [
    z ->
      fun arg fallback ->
        Some (registered_interf arg z)
  ] } in

let ef = EF.{ (ef) with
  signature = extfun ef.signature with [
    z ->
      fun arg fallback ->
        Some (registered_signature arg z)
  ] } in

let ef = EF.{ (ef) with
  structure = extfun ef.structure with [
    z ->
      fun arg fallback ->
        Some (registered_structure arg z)
  ] } in

Pa_passthru.(install { name = "pa_undo_deriving" ; ef = ef ; pass = None ; before = [] ; after = [] })
;

install();
