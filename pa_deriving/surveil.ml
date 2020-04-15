(* camlp5r *)
(* pa_deriving.ml,v *)
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

ef.val := EF.{ (ef.val) with
            str_item = extfun ef.val.str_item with [
    <:str_item:< type $_flag:_$ $list:tdl$ >> as z
    when  List.exists is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      List.iter (fun td -> List.iter (push item_attributes) (Pcaml.unvala td.tdAttributes))
        tdl ;
        None
      }
  ] }
;

ef.val := EF.{ (ef.val) with
            sig_item = extfun ef.val.sig_item with [
    <:sig_item:< type $_flag:_$ $list:tdl$ >> as z
    when  List.exists is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      List.iter (fun td -> List.iter (push item_attributes) (Pcaml.unvala td.tdAttributes))
        tdl ;
        None
      }
  ] }
;

ef.val := EF.{ (ef.val) with
  ctyp = extfun ef.val.ctyp with [
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

  ] }
;

ef.val := EF.{ (ef.val) with
  expr = extfun ef.val.expr with [
    <:expr:< [% $extension:e$ ] >> ->
      fun arg -> do {
        push alg_extensions e ;
        None
      }
  ] }
;

