(* camlp5r *)
(* pa_deriving.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_ppx_utils ;
open Pa_passthru ;
open Ppxutil ;

value extract_option name = (fun [
      (<:patt< $lid:s$ >>, e) -> (s,e)
    | _ -> failwith ("invalid option-label in deriving."^name)
    ])
;

value extract_deriving0 (attr : attribute) =
  match uv attr with [
    <:attribute_body< deriving $structure:sil$ >> ->
      let rv = [] in
      List.fold_left (fun rv -> fun [
        <:str_item< $lid:s$ >> -> [ (s, []) :: rv ]
      | <:str_item< $lid:s$ { $list:l$ } >> ->
        let l = List.map (extract_option s) l in
        [ (s,l) :: rv ]
      | <:str_item< ( $list:l$ ) >> ->
        List.fold_left (fun rv -> fun [
            <:expr< $lid:s$ >>  -> [ (s,[]) :: rv ]
          | <:expr< $lid:s$ { $list:l$ } >> ->
            let l = List.map (extract_option s) l in
            [ (s, l) :: rv ]
          | _ -> rv ]) rv l
      | _ -> rv ]) rv sil
  | _ -> []
  ]
;

value is_deriving_attribute (attr : attribute) = attr_id attr = "deriving" ;

value extract_deriving name attr =
  if not (is_deriving_attribute attr) then None
  else let l = attr |> extract_deriving0
               |> Std.filter (fun (s,_) -> name = s) in
    if l = [] then None
    else  Some (l |> List.map snd |> List.concat)
;

value is_deriving name attr = None <> (extract_deriving name attr) ;

value apply_deriving name ctxt attr =
  let update_ctxt ctxt l =
    Ctxt.{ (ctxt) with options = l @ ctxt.options } in
  match extract_deriving name attr with [
    None -> ctxt
  | Some l -> update_ctxt ctxt l
  ]
;

module PI = struct
type t = {
  name : string
; alternates : list string
; options : list string
; alg_attributes : list string
; expr_extensions : list string
; ctyp_extensions : list string
; expr : Ctxt.t -> expr -> expr
; ctyp : Ctxt.t -> ctyp -> ctyp
; str_item : string -> Ctxt.t -> str_item -> str_item
; sig_item : string -> Ctxt.t -> sig_item -> sig_item
; default_options : list (string * expr)
}
;

value attributes pi = pi.alg_attributes ;

value is_medium_form_attribute pi attr = Std.starts_with ~{pat=pi.name} (attr_id attr) ;
value is_long_form_attribute pi attr =
  let name = Printf.sprintf "deriving.%s" pi.name in
 Std.starts_with ~{pat=name} (attr_id attr) ;

value medium_form_attributes pi =
  List.map (fun n -> Printf.sprintf "%s.%s" pi.name n) (attributes pi)
;
value long_form_attributes pi =
  List.map (fun n -> Printf.sprintf "deriving.%s.%s" pi.name n) (attributes pi)
;

end
;

value plugin_registry = ref [] ;
value alternate2plugin = ref [] ;
value expr_extension2plugin = ref [] ;
value ctyp_extension2plugin = ref [] ;
value algattr2plugin = ref [] ;

value checked_add_assoc ~{mapping} r (k,v) =
  if List.mem_assoc k r.val then
    failwith (Printf.sprintf "%s %s already registered" mapping k)
  else Std.push r (k,v)
;

module Registry = struct
value add t = do {
  checked_add_assoc ~{mapping="plugin"} plugin_registry (t.PI.name, t) ;
  t.PI.alternates |> List.iter (fun s ->
      checked_add_assoc ~{mapping="plugin-alternates"} alternate2plugin (s, t.PI.name)) ;
  t.expr_extensions |> List.iter (fun e ->
      Std.push expr_extension2plugin (e, t.name)) ;

  t.expr_extensions |> List.iter (fun e ->
      Std.push expr_extension2plugin (Printf.sprintf "derive.%s" t.name, t.name)) ;

  t.ctyp_extensions |> List.iter (fun e ->
      Std.push ctyp_extension2plugin (e, t.name)) ;

  t.ctyp_extensions |> List.iter (fun e ->
      Std.push ctyp_extension2plugin (Printf.sprintf "derive.%s" t.name, t.name)) ;

  List.iter (fun aname -> Std.push algattr2plugin (aname, t.name)) t.alg_attributes
}
;

value mem na =
  List.mem_assoc na plugin_registry.val || List.mem_assoc na alternate2plugin.val ;

value get na = match List.assoc na plugin_registry.val with [
  x -> x
| exception Not_found ->
  let na = List.assoc na  alternate2plugin.val in
    List.assoc na plugin_registry.val
]
;
end
;

value registered_str_item (name,pi) arg = fun [
  <:str_item:< type $_flag:_$ $list:_$ >> as z ->
    pi.PI.str_item name arg z

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $itemattrs:_$ >> as z ->
    pi.PI.str_item name arg z

| <:str_item:< exception $_excon:_$ $itemattrs:_$ >> as z ->
    pi.PI.str_item name arg z

| _ -> assert False
]
;

value registered_sig_item (name,pi) arg = fun [
  <:sig_item:< type $_flag:_$ $list:_$ >> as z ->
    pi.PI.sig_item name arg z

| <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $itemattrs:_$ >> as z ->
    pi.PI.sig_item name arg z

| _ -> assert False
]
;

value registered_expr_extension arg = fun [
  <:expr:< [% $_extension:e$ ] >> as z ->
    let ename = attr_id e in
    let piname = List.assoc ename expr_extension2plugin.val in
    let pi = List.assoc piname plugin_registry.val in
    let arg = Ctxt.add_options arg pi.default_options in
    Some (pi.expr arg z)
| _ -> assert False
]
;

value registered_ctyp_extension arg = fun [
  <:ctyp:< [% $_extension:e$ ] >> as z ->
    let ename = attr_id e in
    let piname = List.assoc ename ctyp_extension2plugin.val in
    let pi = List.assoc piname plugin_registry.val in
    let arg = Ctxt.add_options arg pi.default_options in
    Some (pi.ctyp arg z)
| _ -> assert False
]
;

value is_registered_deriving attr =
  List.exists (fun (name, _) -> is_deriving name attr) plugin_registry.val ;

value is_registered_expr_extension attr =
  List.mem_assoc (attr_id attr) expr_extension2plugin.val ;

value is_registered_ctyp_extension attr =
  List.mem_assoc (attr_id attr) ctyp_extension2plugin.val ;

value is_registered_plugin na =
  List.mem_assoc na plugin_registry.val || List.mem_assoc na alternate2plugin.val ;

value rewrite_deriving_attribute attrs =
  let (deriving_attr, other_attrs) =
    match filter_split is_deriving_attribute attrs with [
      (([] | [_ ; _ :: _]), _) -> failwith "should only be one @@deriving attribute"
    | ([a], others) -> (a, others)
    ] in
  let (loc_attrid, payload) = uv deriving_attr in
  let idloc = fst (uv loc_attrid) in
  let newattr = (<:vala< (idloc, "deriving_inline") >>, payload) in
  other_attrs @ [ <:vala< newattr >> ]
;

value rewrite_str_item_deriving_attribute = fun [
  <:str_item:< type $flag:nrfl$ $list:tdl$ >> as z ->
    let (last, tdl) = sep_last tdl in
    let attrs = rewrite_deriving_attribute (uv last.tdAttributes) in
    let last = { (last) with tdAttributes = <:vala< attrs >> } in
    let tdl = tdl @ [ last ] in
    <:str_item:< type $flag:nrfl$ $list:tdl$ >>

| <:str_item:< type $lilongid:lili$ $_list:pl$ += $_priv:pf$ [ $list:ecs$ ] $itemattrs:attrs$ >> ->
    let attrs = rewrite_deriving_attribute attrs in
    <:str_item:< type $lilongid:lili$ $_list:pl$ += $_priv:pf$ [ $list:ecs$ ] $itemattrs:attrs$ >>

| <:str_item:< exception $_excon:ec$ $itemattrs:attrs$ >> ->
    let attrs = rewrite_deriving_attribute attrs in
    <:str_item< exception $_excon:ec$ $itemattrs:attrs$ >>
]
;

value invoke_str_item_plugin arg z (na, options) =
  let loc = Ploc.dummy in 
  if not (is_registered_plugin na) then
    if List.mem_assoc "optional" options &&
       Reloc.eq_expr <:expr< True >> (List.assoc "optional" options) then []
    else failwith (Printf.sprintf "Pa_deriving.str_item: missing but mandatory plugin %s" na)
  else
    let pi = Registry.get na in
    let arg = Ctxt.add_options arg pi.PI.default_options in
    let arg = Ctxt.add_options arg options in
    let arg = Ctxt.add_options arg [("plugin_name", <:expr< $str:na$ >>)] in
    [registered_str_item (na,pi) arg z]
;

value rewrite_sig_item_deriving_attribute = fun [
  <:sig_item:< type $flag:nrfl$ $list:tdl$ >> as z ->
    let (last, tdl) = sep_last tdl in
    let attrs = rewrite_deriving_attribute (uv last.tdAttributes) in
    let last = { (last) with tdAttributes = <:vala< attrs >> } in
    let tdl = tdl @ [ last ] in
    <:sig_item:< type $flag:nrfl$ $list:tdl$ >>

| <:sig_item:< type $lilongid:lili$ $_list:pl$ += $_priv:pf$ [ $list:ecs$ ] $itemattrs:attrs$ >> ->
    let attrs = rewrite_deriving_attribute attrs in
    <:sig_item:< type $lilongid:lili$ $_list:pl$ += $_priv:pf$ [ $list:ecs$ ] $itemattrs:attrs$ >>
]
;

value invoke_sig_item_plugin arg z (na, options) =
  let loc = Ploc.dummy in 
  if not (is_registered_plugin na) then
    if List.mem_assoc "optional" options &&
       Reloc.eq_expr <:expr< True >> (List.assoc "optional" options) then []
    else failwith (Printf.sprintf "Pa_deriving.str_item: missing but mandatory plugin %s" na)
  else
    let pi = Registry.get na in
    let arg = Ctxt.add_options arg pi.PI.default_options in
    let arg = Ctxt.add_options arg options in
    let arg = Ctxt.add_options arg [("plugin_name", <:expr< $str:na$ >>)] in
    [registered_sig_item (na,pi) arg z]
;

value install () =
let ef = EF.mk() in
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< type $flag:nrfl$ $list:tdl$ >> as z
    when 1 = count is_deriving_attribute (uv (fst (sep_last tdl)).tdAttributes) ->
    fun arg _ ->
      let last_td = fst (sep_last tdl) in
      let attrs = last_td.tdAttributes in
      let derivings = attrs |> uv |> List.map extract_deriving0 |> List.concat in
      let l = derivings |> List.map (invoke_str_item_plugin arg z) |> List.concat in
      let z = rewrite_str_item_deriving_attribute z in
      let l = [z :: l ] @ [ <:str_item< [@@@"end"] >> ] in
      Some <:str_item< declare $list:l$ end >>

  | <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $itemattrs:attrs$ >> as z
    when 1 = count is_deriving_attribute attrs ->
    fun arg _ ->
      let derivings = attrs |> List.map extract_deriving0 |> List.concat in
      let l = derivings |> List.map (invoke_str_item_plugin arg z) |> List.concat in
      let z = rewrite_str_item_deriving_attribute z in
      let l = [z :: l ] @ [ <:str_item< [@@@"end"] >> ] in
      Some <:str_item< declare $list:l$ end >>

  | <:str_item:< exception $_excon:_$ $itemattrs:attrs$ >> as z
    when 1 = count is_deriving_attribute attrs ->
    fun arg _ ->
      let derivings = attrs |> List.map extract_deriving0 |> List.concat in
      let l = derivings |> List.map (invoke_str_item_plugin arg z) |> List.concat in
      let z = rewrite_str_item_deriving_attribute z in
      let l = [z :: l ] @ [ <:str_item< [@@@"end"] >> ] in
      Some <:str_item< declare $list:l$ end >>
      
  ] } in

let ef = EF.{ (ef) with
            sig_item = extfun ef.sig_item with [
    <:sig_item:< type $_flag:_$ $list:tdl$ >> as z
    when 1 = count is_deriving_attribute (uv (fst (sep_last tdl)).tdAttributes) ->
    fun arg _ ->
      let last_td = fst (sep_last tdl) in
      let attrs = last_td.tdAttributes in
      let derivings = attrs |> uv |> List.map extract_deriving0 |> List.concat in
      let l = derivings |> List.map (invoke_sig_item_plugin arg z) |> List.concat in
      let z = rewrite_sig_item_deriving_attribute z in
      let l = [z :: l ] @ [ <:sig_item< [@@@"end"] >> ] in
      Some <:sig_item< declare $list:l$ end >>

  | <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $itemattrs:attrs$ >> as z
    when 1 = count is_deriving_attribute attrs ->
    fun arg _ ->
      let derivings = attrs |> List.map extract_deriving0 |> List.concat in
      let l = derivings |> List.map (invoke_sig_item_plugin arg z) |> List.concat in
      let z = rewrite_sig_item_deriving_attribute z in
      let l = [z :: l ] @ [ <:sig_item< [@@@"end"] >> ] in
      Some <:sig_item< declare $list:l$ end >>

  ] } in

let ef = EF.{ (ef) with
  expr = extfun ef.expr with [
    <:expr:< [% $_extension:e$ ] >> as z when is_registered_expr_extension e ->
      fun arg _ ->
        registered_expr_extension arg z
  ] } in

let ef = EF.{ (ef) with
  ctyp = extfun ef.ctyp with [
    <:ctyp:< [% $_extension:e$ ] >> as z when is_registered_ctyp_extension e ->
      fun arg _ ->
        registered_ctyp_extension arg z
  ] } in

  Pa_passthru.(install { name = "pa_deriving" ; ef = ef ; pass = None ; before = [] ; after = ["pa_import";"surveil"] })
;
