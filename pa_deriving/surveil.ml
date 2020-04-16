(* camlp5r *)
(* surveil.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_passthru ;
open Ppxutil ;
open Pa_deriving ;

value ef = ref (EF.mk ()) ;

value item_attributes = ref [] ;
value alg_attributes = ref [] ;
value alg_extensions = ref [] ;

value install_to_ef ef =
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< type $_flag:_$ $list:tdl$ >>
    when  List.exists is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      List.iter (fun td -> List.iter (push item_attributes) (Pcaml.unvala td.tdAttributes))
        tdl ;
        None
      }
  ] } in

let ef = EF.{ (ef) with
            sig_item = extfun ef.sig_item with [
    <:sig_item:< type $_flag:_$ $list:tdl$ >>
    when  List.exists is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      List.iter (fun td -> List.iter (push item_attributes) (Pcaml.unvala td.tdAttributes))
        tdl ;
        None
      }
  ] } in

let ef = EF.{ (ef) with
  ctyp = extfun ef.ctyp with [
    <:ctyp:< $_$ [@ $attribute:attr$ ] >> ->
      fun arg -> do {
        push alg_attributes attr ;
        None
      }
  | <:ctyp:< [ $list:l$ ] >> ->
      fun arg -> do {
        List.iter (fun [
          (loc, cid, tyl, None, attrs) ->
          List.iter (fun a -> push alg_attributes (Pcaml.unvala a)) (Pcaml.unvala attrs)
        | _ -> ()
        ]) l ;
        None
      }

  ] } in

let ef = EF.{ (ef) with
  expr = extfun ef.expr with [
    <:expr:< [% $extension:e$ ] >> ->
      fun arg -> do {
        push alg_extensions e ;
        None
      }
  ] } in
ef
;
