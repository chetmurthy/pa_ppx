(* camlp5r *)
(* surveil.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
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
  if not (List.mem s r.val) then Std.push r s else ()
;
value addsetl r l = List.iter (addset r) l ;
type form_t = [ Short | Medium | Long ] ;
type t =
  {
    all_plugins : ref (list string)
  ; all_attributes : ref (list string)

  ; current_plugins : ref (list string)
  ; current_attributes : ref (list string)

  ; allowed_form : ref (option form_t)
  }
;
value mk () = { 
  all_plugins = ref []
; all_attributes = ref [] 
; current_plugins = ref []
; current_attributes = ref []
; allowed_form = ref None
} ;

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

value legitimate_plugin_reference dc (na, options) =
  match Registry.get na with [
    pi ->
    List.for_all (fun (oname,_) -> List.mem oname pi.PI.options) options
  | exception Not_found ->
    List.exists (fun [ ("optional", <:expr< True >>) -> True | _ -> False ]) options
  ]
;

value start_decl dc plugins = do {
  assert ([] = dc.current_plugins.val) ;
  assert ([] = dc.current_attributes.val) ;
  List.iter (fun ((na, _) as r) ->
      if not (legitimate_plugin_reference dc r) then
        failwith (Printf.sprintf "ill-formed plugin reference %s" na)
      else ()) plugins ;
  let plugins = Std.filter (fun (na,_) -> Registry.mem na) plugins in
  dc.current_plugins.val := List.map fst plugins ;
  plugins
}
;

value end_decl dc = do {
  let attributes = dc.current_attributes.val in
  dc.current_plugins.val := [] ;
  dc.current_attributes.val := [] ;
  attributes
}
;

value set_form dc f =
  if dc.allowed_form.val = None then
    dc.allowed_form.val := Some f
  else if dc.allowed_form.val = Some f then ()
  else failwith "DC.set_form: form of attributes/extensions already set; trying to set it to different value"
;

value get_form dc =
  match dc.allowed_form.val with [ None -> Short | Some f -> f ] ;

value (dump : Fmt.t t) ofmt dc =
  let ssl = Fmt.(list ~{sep=semi} string) in
  let ppform ppf = fun [
    Short -> Fmt.(const string "Short" ppf ())
  | Medium -> Fmt.(const string "Medium" ppf ())
  | Long -> Fmt.(const string "Lon" ppf ()) ] in
  Fmt.(pf ofmt "<dc< {@[ @[all_plugins = [ %a ];@]@, @[all_attributes = [ %a ];@]@, @[current_plugins = [ %a ]@] @[current_attributes = [ %a ];@]@, @[allowed_form = %a@] } >>@.%!"
         ssl dc.all_plugins.val
        ssl dc.all_attributes.val
        ssl dc.current_plugins.val
        ssl dc.current_attributes.val
        (option ppform) dc.allowed_form.val
      )
;

value allowed_attribute dc piname attrname = do {
  assert (List.mem attrname  Registry.((get piname).alg_attributes)) ;
  match dc.allowed_form.val with [
    (None | Some Short) -> attrname
  | Some Medium -> Printf.sprintf "%s.%s" piname attrname
  | Some Long -> Printf.sprintf "deriving.%s.%s" piname attrname
  ]
}
;
value is_allowed_attribute dc piname attrname attr =
  let wantid = allowed_attribute dc piname attrname in
  wantid = attr_id attr
;
end ;

module DC = DerivingConfig ;  

value implem arg x = do {
  DC.init arg ;
  Some (Pa_passthru.implem0 arg x)
}
;

value add_current_attribute arg id =
  let dc = DC.get arg in
  DC.addset dc.current_attributes id
;

value add_deriving_attributes ctxt attrs = do {
    let dc = DC.get ctxt in
    let attrs = Std.filter is_deriving_attribute attrs in
    let plugins = extract_deriving0 (List.hd attrs) in
    if plugins = [] then failwith "Surveil.str_item: @@deriving with no plugins"
    else DC.addsetl dc.all_plugins (List.map fst plugins) ;
    plugins
}
;

value sig_item arg = fun [
  <:sig_item:< type $_flag:_$ $list:tdl$ >> as z -> do {
    let td = fst (sep_last tdl) in
    let plugins = add_deriving_attributes arg (Pcaml.unvala td.tdAttributes) in
    let dc = DC.get arg in
    let plugins = DC.start_decl dc plugins in
    let rv = Pa_passthru.sig_item0 arg z in
    let attributes = DC.end_decl dc in
    let reg_short_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.attributes
      |> List.concat in
    let reg_short_form_duplicated = duplicated reg_short_form_attributes in
    let reg_medium_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.medium_form_attributes
      |> List.concat in
    let reg_long_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.long_form_attributes
      |> List.concat in

    let short_form_attributes = Std.intersect attributes reg_short_form_attributes in
    let medium_form_attributes = Std.intersect attributes reg_medium_form_attributes in
    let long_form_attributes = Std.intersect attributes reg_long_form_attributes in

    if not (match (short_form_attributes<>[], medium_form_attributes<>[], long_form_attributes<>[]) with [
      (True, False, False) -> True
    | (False, True, False) -> True
    | (False, False, True) -> True
    | (False, False, False) -> True
    | _ -> False
    ]) then failwith "mixed short/medium/long-form attributes"
    else () ;
    if short_form_attributes <> [] && reg_short_form_duplicated then
      failwith "short-form attributes used, but some apply to more than one plugin"
    else () ;
    
    if [] <> long_form_attributes then DC.(set_form dc Long)
    else if [] <> medium_form_attributes then DC.(set_form dc Medium)
    else if [] <> short_form_attributes then DC.(set_form dc Short)
    else () ;
    rv
  }
| _ -> assert False
]
;

value str_item arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> as z -> do {
    let td = fst (sep_last tdl) in
    let plugins = add_deriving_attributes arg (Pcaml.unvala td.tdAttributes) in
    let dc = DC.get arg in
    let plugins = DC.start_decl dc plugins in
    let rv = Pa_passthru.str_item0 arg z in
    let attributes = DC.end_decl dc in
    let reg_short_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.attributes
      |> List.concat in
    let reg_medium_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.medium_form_attributes
      |> List.concat in
    let reg_long_form_attributes =
      plugins
      |> List.map fst
      |> List.map Registry.get
      |> List.map PI.long_form_attributes
      |> List.concat in

    let used_short_form_attributes = Std.filter (fun s -> List.mem s attributes) reg_short_form_attributes in
    let used_medium_form_attributes = Std.filter (fun s -> List.mem s attributes) reg_medium_form_attributes in
    let used_long_form_attributes = Std.filter (fun s -> List.mem s attributes) reg_long_form_attributes in

    if not (match (used_short_form_attributes<>[],
                   used_medium_form_attributes<>[],
                   used_long_form_attributes<>[]) with [
      (True, False, False) -> True
    | (False, True, False) -> True
    | (False, False, True) -> True
    | (False, False, False) -> True
    | _ -> False
    ]) then failwith "mixed short/medium/long-form attributes"
    else () ;
    if duplicated used_short_form_attributes then
      failwith "short-form attributes used, but some apply to more than one plugin"
    else () ;
    
    if [] <> used_long_form_attributes then DC.(set_form dc Long)
    else if [] <> used_medium_form_attributes then DC.(set_form dc Medium)
    else if [] <> used_short_form_attributes then DC.(set_form dc Short)
    else () ;
    rv
  }
| _ -> assert False
]
;

value install () =
let ef = EF.mk() in

let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< type $_flag:_$ $list:tdl$ >> as z
    when  1 = count is_deriving_attribute (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> Some (str_item arg z)
  ] } in
let ef = EF.{ (ef) with
            sig_item = extfun ef.sig_item with [
    <:sig_item:< type $_flag:_$ $list:tdl$ >> as z
    when  1 = count is_deriving_attribute (Pcaml.unvala (fst (sep_last tdl)).tdAttributes) ->
    fun arg -> Some (sig_item arg z)
  ] } in

let ef = EF.{ (ef) with
  ctyp = extfun ef.ctyp with [
    <:ctyp:< $_$ [@ $_attribute:attr$ ] >> ->
      fun arg -> do {
        add_current_attribute arg (attr_id attr) ;
        None
      }
  | <:ctyp:< [ $list:l$ ] >> ->
      fun arg -> do {
        List.iter (fun [
          (loc, cid, tyl, None, attrs) ->
          List.iter (fun a -> add_current_attribute arg (attr_id a)) (Pcaml.unvala attrs)
        | _ -> ()
        ]) l ;
        None
      }

  ] } in

let ef = EF.{ (ef) with
  implem = extfun ef.implem with [
    z ->
      fun arg ->
        let rv = implem arg z in do {
        Fmt.(DC.dump stderr (DC.get arg)) ; rv }
  ] } in
  Pa_passthru.(install { name = "surveil" ; ef = ef ; before = [] ; after = [] })
;
