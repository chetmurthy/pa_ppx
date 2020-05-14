(* camlp5r *)
(* pa_here.ml,v *)
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

value expr_to_longident eli =
  let rec erec = fun [
    <:expr:< $uid:s$ >> -> <:longident< $uid:s$ >>
  | <:expr:< $e1$ . $uid:s$ >> -> <:longident< $longid:erec e1$ . $uid:s$ >>
  | <:expr:< $e1$ . $e2$ >> ->
    match e2 with [
      <:expr:< $e2$ . $e3$ >> ->
        let e1 = <:expr< $e1$ . $e2$ >> in
        erec <:expr< $e1$ . $e3$ >>
      | _ -> Ploc.raise loc (Failure "expr_to_longident: bad expr")
    ]
  ]
  in erec eli
;
value is_rebind_to_attribute (attr : attribute) = attr_id attr = "rebind_to" ;

value rebind_extension_constructor arg = fun [
   EcTuple (loc, ci, _, None, attrs) as z
    when List.exists is_rebind_to_attribute (uv attrs) ->
    let (rebind_attrs, others) = filter_split is_rebind_to_attribute (uv attrs) in
    let eli = match List.map uv rebind_attrs  with [
      [ <:attribute_body:< rebind_to $exp:eli$ ; >> ] -> eli
    | _ -> Ploc.raise loc (Failure "rebind_extension_constructor: bad rebind_to attribute")
    ] in
    <:extension_constructor< $_uid:ci$ = $longid:expr_to_longident eli$ $algattrs:others$ >>
 ]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            extension_constructor = extfun ef.extension_constructor with [
    <:extension_constructor:< $uid:ci$ of $list:_$ $algattrs:attrs$ >> as z
    when List.exists is_rebind_to_attribute attrs ->
    fun arg ->
      Some (rebind_extension_constructor arg z)
  | <:extension_constructor:< $uid:ci$  $algattrs:attrs$ >> as z
    when List.exists is_rebind_to_attribute attrs ->
    fun arg ->
      Some (rebind_extension_constructor arg z)
  ] } in
  Pa_passthru.(install { name = "pa_rebindto"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"] })
;

install();
