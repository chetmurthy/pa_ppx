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

  (** Attributes and extensions in deriving come in three forms:

  @opaque, %printer -- short-form

  @show.opaque, %show.printer -- medium-form

  @deriving.show.opaque, @deriving.show.printer -- long-form

  Surveil computes which form to demand of @@deriving plugins.  The
     rule is that if any extension or attribute registered to more
     than one plugin is used in short-form, then ALL plugins must use
     medium-form.

  It is ALWAYS permitted to use long-form attributes/extensions.

*)

(** Surveil scans the entire file, looking for:

  (a) @@driving -- and records eery plugin that gets invoked

  (b) %extension -- and records every one

  (c) @attr -- and records every one, and specifically for those that appear within
      an @@deriving, it records which @@deriving they were beneath

After scanning, Surveil computes the following:

  (1) if there are any plugins invoked, but not loaded -- and for these,
      if they're NOT invoked with @optional=true in which case an error is raised

  (2) if there are any invoked short-form extensions that are registered to more than
      one plugin: again this is an error

*)


module DerivingConfig = struct
value addset r s =
  if not (List.mem s r.val) then push r s else ()
;
type t =
  {
    all_plugins : ref (list string)
  ; current_plugins : ref (list string)
  ; attributes : ref (list string)
  ; extensions : ref (list string)
  }
;
value mk () = { all_plugins = ref [] ; current_plugins = ref [] ;
                attributes = ref [] ; extensions = ref [] } ;

type scratchdata_t += [ Pa_deriving of t ] ;

value get arg =
  match Ctxt.scratchdata arg "deriving" with [
    Pa_deriving dc -> dc
  | _ -> assert False
  ]
;
value init arg =
   Ctxt.init_scratchdata arg "deriving" (Pa_deriving (mk()))
;

value dump ofmt dc =
  let ssl = Fmt.(list ~{sep=semi} string) in
  Fmt.(pf ofmt "<dc< { all_plugins = [ %a ]; current_plugins = [ %a ] ; attributes = [ %a ] ; extension = [ %a ] >>%!"
         ssl dc.all_plugins.val ssl dc.current_plugins.val ssl dc.attributes.val ssl dc.extensions.val)
;
end
;
module DC = DerivingConfig ;  

value implem arg x = do {
  DC.init arg ;
  Some (Pa_passthru.implem0 arg x)
}
;

value add_deriving arg attr =
  let dc = DC.get arg in
  let _ = dc in
  let l = extract_deriving0 attr in
  if l = [] then failwith "add_derivings: @@deriving with no plugins"
  else
    l |> List.map fst |> List.iter (DC.addset dc.all_plugins)
;



value install () =
let ef = EF.mk() in

let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< type $_flag:_$ $list:tdl$ >>
    when  1 = count is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      let add1 (attr : attribute_body) = if is_deriving_attribute attr then
          add_deriving arg attr else () in
      List.iter add1 (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ;
        None
      }
  ] } in

let ef = EF.{ (ef) with
            sig_item = extfun ef.sig_item with [
    <:sig_item:< type $_flag:_$ $list:tdl$ >>
    when  List.exists is_registered_deriving (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> do {
      let add1 (attr : attribute_body) = if is_deriving_attribute attr then
          add_deriving arg attr else () in
      List.iter add1 (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ;
        None
      }
  ] } in
(*
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
*)
let ef = EF.{ (ef) with
  implem = extfun ef.implem with [
    z ->
      fun arg ->
        let rv = implem arg z in do {
        Fmt.(DC.dump stderr (DC.get arg)) ; rv }
  ] } in
  Pa_passthru.install ("surveil", ef)
;
