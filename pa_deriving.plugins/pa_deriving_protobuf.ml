(* camlp5r *)
(* pa_deriving_protobuf.ml,v *)
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
open Pa_deriving_base ;

module Ctxt = struct
  include Pa_passthru.Ctxt ;

  value plugin_name ctxt = do {
    assert (List.mem_assoc "plugin_name" ctxt.options) ;
    match List.assoc "plugin_name" ctxt.options with [
      <:expr< $str:na$ >> -> na
    | _ -> assert False
    ]}
  ;
  value is_plugin_name ctxt s = (s = plugin_name ctxt) ;

  value bare_types ctxt = do {
    assert (List.mem_assoc "bare" ctxt.options) ;
    match List.assoc "bare" ctxt.options with [
      <:expr< () >> -> []
    | <:expr:< $lid:l$ >> -> [l]
    | <:expr:< ( $list:l$ ) >> ->
      List.map (fun [ <:expr< $lid:t$ >> -> t
                    | _ -> Ploc.raise loc (Failure "protobuf: bare option must be given lidents") ]) l
    ]
  } ;

  value is_bare ctxt s = List.mem s (bare_types ctxt) ;
end ;

value tuplectyp loc l = if List.length l = 1 then List.hd l else <:ctyp< ( $list:l$ ) >> ;
value tuplepatt loc l = if List.length l = 1 then List.hd l else <:patt< ( $list:l$ ) >> ;
value tupleexpr loc l = if List.length l = 1 then List.hd l else <:expr< ( $list:l$ ) >> ;

type encoding_t = [ Varint | Zigzag | Bits32 | Bits64 ] ;
type attrmod_t = {
  encoding : option encoding_t
; key : option int
; message : bool
; bare : bool
; param : bool
; arity : option [ = `List | `Array | `Optional ]
; packed : bool
; default : option MLast.expr
; field_name : string
} ;

value init_attrmod fld = {
  encoding = None
; key = None
; message = False
; bare = False
; param = False
; arity = None
; packed = False
; default = None
; field_name = fld
} ;
value attrmod_key a = match a.key with [ None -> 1 | Some n -> n ] ;
value attrmod_key_compare a b = Int.compare (attrmod_key a) (attrmod_key b) ;
value fmt_attrmod_key a = a |> attrmod_key |> string_of_int ;

value attrs_to_key loc arg attrs =
  match List.map uv (List.find_all (DC.is_allowed_attribute (DC.get arg) "protobuf" "key") (uv attrs)) with [
    [  <:attribute_body< $_attrid:_$ $int:key$; >> ] -> int_of_string key
  | _ -> Ploc.raise loc (Failure "MUST specify exactly one @key")
  ]
;

value format_msg ctxt field_name =
  Printf.sprintf "%s.%s" (Ctxt.module_path_s ctxt) field_name ;

module Variant = struct

type preprocessed_t 'a = {
  it : 'a ;
  slotnum : option int ;
  oldkey : int ;
  newkey : int ;
  arity : option [ = `List | `Array | `Optional ]
} ;

(** preprocess a variant type, returning a list of:

  [preprocessed_t branch]

  old key is the original key declared by the branch
  new key is 1+ that (for convenience)
  slotnum is the slot for this branch in the generated tuple type:
    None if this is a branch without arguments (no slot)
    Some n for the n-th slot.
*)
value preprocess loc arg attrmod l =
  let (rev_branch_slotnums, nextslot) = List.fold_left (fun (acc, slotnum) -> fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, attrs) -> assert False

  | (loc, cid, <:vala< [] >>, None, attrs) as b ->
    let key = attrs_to_key loc arg attrs in
    ([ {it=b;  oldkey=key; newkey=key+1; arity=None; slotnum=None} :: acc], slotnum)

  | (loc, cid, tyl, None, attrs) as b ->
    let key = attrs_to_key loc arg attrs in
    let arity = match uv tyl with [
      [ <:ctyp< option $_$ >> ] -> Some `Optional
    | [ <:ctyp< list $_$ >> ] -> Some `List
    | [ <:ctyp< array $_$ >> ] -> Some `Array
    | _ -> None
    ] in
    ([ {it=b; oldkey=key; newkey=key+1; arity=arity; slotnum=Some slotnum} :: acc], slotnum+1)
  ]) ([], 1) l in
  let branch_key_slotnums = List.rev rev_branch_slotnums in
  (branch_key_slotnums, nextslot-1)
;

(** convert a variant type into a tuple type.

   By reference to the following type, I'll describe what we need:

   type t = A [@key 1] | B [@key 2] | C of int [@key 3] | D [@key 4] | E of int * string [@key 5]

   Each branch (MUST) has a key.  We must compute a "slot number"
   for each branch in the tuple-type, which will be:

   (int[@key 1] * option int [@key 3+1] * option(int * string) [@key 5+1])

   Branches with arguments get slot-numbers.  All branches get keys (1+ the declared key).

   The first int take the value of the original branch keys.
*)
value to_tupletype loc arg branch_key_slotnums =
  let totuple_branch2tuplety = fun [
    {it=(loc, cid, <:vala< [TyRec _ fields] >>, None, attrs)} -> assert False

  | {it=(loc, cid, <:vala< [] >>, None, attrs)} ->
    []

  | {it=(loc, cid, tyl, None, attrs); newkey=newkey; arity = None} ->
    let branchtuplety = tuplectyp loc (uv tyl) in
    [<:ctyp< option $branchtuplety$ [@key $int:string_of_int newkey$; ] >>]

  | {it=(loc, cid, tyl, None, attrs); newkey=newkey; arity = Some _} ->
    let branchtuplety = tuplectyp loc (uv tyl) in
    [<:ctyp< $branchtuplety$ [@key $int:string_of_int newkey$; ] >>]

  ] in
    tuplectyp loc [ <:ctyp< int [@ key 1;] >> :: (List.concat (List.map totuple_branch2tuplety branch_key_slotnums)) ]
;

value all_empty_expr loc branch_key_slotnums =
  branch_key_slotnums |> List.map (fun [
    {slotnum=None} -> []
  | {arity=None} -> [<:expr< None >>]
  | {arity=Some `Optional} -> [<:expr< None >>]
  | {arity=Some `List} -> [<:expr< [] >>]
  | {arity=Some `Array} -> [<:expr< [||] >>]
  ])
  |> List.concat
;

value all_empty_patt loc branch_key_slotnums =
  branch_key_slotnums |> List.map (fun [
    {slotnum=None} -> []
  | {arity=None} -> [<:patt< None >>]
  | {arity=Some `Optional} -> [<:patt< None >>]
  | {arity=Some `List} -> [<:patt< [] >>]
  | {arity=Some `Array} -> [<:patt< [||] >>]
  ])
  |> List.concat
;

value empty_except_i_expr loc branch_key_slotnums (k,e) =
  branch_key_slotnums |> List.map (fun [
    {slotnum=None} -> []
  | {slotnum=Some slotnum} when k = slotnum -> [e]
  | {arity=None} -> [<:expr< None >>]
  | {arity=Some `Optional} -> [<:expr< None >>]
  | {arity=Some `List} -> [<:expr< [] >>]
  | {arity=Some `Array} -> [<:expr< [||] >>]
  ])
  |> List.concat
;
value empty_except_i_patt loc branch_key_slotnums (k,e) =
  branch_key_slotnums |> List.map (fun [
    {slotnum=None} -> []
  | {slotnum=Some slotnum} when k = slotnum -> [e]
  | {arity=None} -> [<:patt< None >>]
  | {arity=Some `Optional} -> [<:patt< None >>]
  | {arity=Some `List} -> [<:patt< [] >>]
  | {arity=Some `Array} -> [<:patt< [||] >>]
  ])
  |> List.concat
;

(** convert a variant value into a tuple. *)
value totuple loc arg argvar (branch_key_slotnums, nslots) =

  let all_empty_expr = all_empty_expr loc branch_key_slotnums in

  let totuple_branch2tuple_expr argvars = fun [
    {it=(loc, cid, <:vala< [TyRec _ fields] >>, None, attrs)} -> assert False

  | {it=(loc, cid, <:vala< [] >>, None, attrs); oldkey=oldkey; slotnum=None} ->
    tupleexpr loc [ <:expr< $int:string_of_int oldkey$ >> :: all_empty_expr ]

  | {it=(loc, cid, tyl, None, attrs); oldkey=oldkey; slotnum=Some slotnum; arity=arity} ->
    (* (Some i, None ... None ..., Some <tuple-of-argvars>, None ...) *)
    let argtuple_expr =  tupleexpr loc (List.map (fun v -> <:expr< $lid:v$ >>) argvars) in
    let arg_expr = match arity with [
      None -> <:expr< Some $argtuple_expr$ >>
    | Some _ -> argtuple_expr
    ] in
    tupleexpr loc [ <:expr< $int:string_of_int oldkey$ >>
      :: empty_except_i_expr loc branch_key_slotnums (slotnum, arg_expr) ]
  ] in

  let totuple_branch2branch_patt = fun [
    {it=(loc, cid, <:vala< [TyRec _ fields] >>, None, attrs)} -> assert False

  | {it=(loc, cid, <:vala< [] >>, None, attrs); oldkey=oldkey; slotnum=None} ->
    (<:patt< $uid:uv cid$ >>, [])

  | {it=(loc, cid, tyl, None, attrs); oldkey=oldkey; slotnum=Some slotnum} ->
    let argvars = List.mapi (fun i _ -> Printf.sprintf "v_%d" i) (uv tyl) in
    (Patt.applist <:patt< $uid:uv cid$ >> (List.map (fun v -> <:patt< $lid:v$ >>) argvars),
     argvars)

  ] in

  let totuple_branches = List.map (fun bks ->
    let (patt,argvars) = totuple_branch2branch_patt bks in
    let expr = totuple_branch2tuple_expr argvars bks in
    (patt, <:vala< None >>, expr)) branch_key_slotnums in
  <:expr< match v with [ $list:totuple_branches$ ] >>
;

value oftuple loc arg ~{field_name} (branch_key_slotnums, nslots) =
  let msg = format_msg arg field_name in
  let all_empty_patt = all_empty_patt loc branch_key_slotnums in

  let oftuple_branch2tuple_patt argvars = fun [
    {it=(loc, cid, <:vala< [TyRec _ fields] >>, None, attrs)} -> assert False

  | {it=(loc, cid, <:vala< [] >>, None, attrs); oldkey=oldkey; slotnum=None} ->
    tuplepatt loc [ <:patt< $int:string_of_int oldkey$ >> :: all_empty_patt ]

  | {it=(loc, cid, tyl, None, attrs); oldkey=oldkey; slotnum=Some slotnum; arity=arity} ->
    (* (Some i, None ... None ..., Some <tuple-of-argvars>, None ...) *)
    let argtuple_patt =  tuplepatt loc (List.map (fun v -> <:patt< $lid:v$ >>) argvars) in
    let arg_patt = match arity with [
      None -> <:patt< Some $argtuple_patt$ >>
    | Some _ -> argtuple_patt
    ] in
    tuplepatt loc [ <:patt< $int:string_of_int oldkey$ >>
      :: empty_except_i_patt loc branch_key_slotnums (slotnum, arg_patt) ]
  ] in

  let oftuple_branch2branch_expr = fun [
    {it=(loc, cid, <:vala< [TyRec _ fields] >>, None, attrs)} -> assert False

  | {it=(loc, cid, <:vala< [] >>, None, attrs); oldkey=oldkey; slotnum=None} ->
    (<:expr< $uid:uv cid$ >>, [])

  | {it=(loc, cid, tyl, None, attrs); oldkey=oldkey; slotnum=Some slotnum} ->
    let argvars = List.mapi (fun i _ -> Printf.sprintf "v_%d" i) (uv tyl) in
    (Expr.applist <:expr< $uid:uv cid$ >> (List.map (fun v -> <:expr< $lid:v$ >>) argvars),
     argvars)

  ] in

  let oftuple_branches = List.map (fun bks ->
    let (expr,argvars) = oftuple_branch2branch_expr bks in
    let patt = oftuple_branch2tuple_patt argvars bks in
    (patt, <:vala< None >>, expr)) branch_key_slotnums in
  let oftuple_branches = oftuple_branches @ [
    (<:patt< _ >>, <:vala< None >>,
     <:expr< raise
             (let open Protobuf.Decoder in
                Failure (Malformed_variant $str:msg$)) >>)
  ] in
  <:expr< fun [ $list:oftuple_branches$ ] >>
;

end ;

module PVariant = struct

type preprocessed_t 'a = Variant.preprocessed_t 'a == {
  it : 'a ;
  slotnum : option int ;
  oldkey : int ;
  newkey : int ;
  arity : option [ = `List | `Array | `Optional ]
} ;

(** preprocess a pvariant type, returning a list of:

  branch * (old key, new key) * slotnum

  old key is the original key declared by the branch
  new key is 1+ that (for convenience)
  slotnum is the slot for this branch in the generated tuple type:
    None if this is a branch without arguments (no slot)
    Some n for the n-th slot.
*)
value preprocess loc arg attrmod l =
  let (rev_branch_slotnums, nextslot) = List.fold_left (fun (acc, slotnum) -> fun [
    (PvTag loc cid _ <:vala< [] >> attrs) as b ->
    let key = attrs_to_key loc arg attrs in
    ([ {it=b; oldkey=key; newkey=key+1; arity=None; slotnum=None} :: acc], slotnum)

  | (PvTag loc cid _ tyl attrs) as b ->
    let key = attrs_to_key loc arg attrs in
    let arity = match uv tyl with [
      [ <:ctyp< option $_$ >> ] -> Some `Optional
    | [ <:ctyp< list $_$ >> ] -> Some `List
    | [ <:ctyp< array $_$ >> ] -> Some `Array
    | _ -> None
    ] in
    ([ {it=b; oldkey=key; newkey=key+1; arity=arity; slotnum=Some slotnum} :: acc], slotnum+1)
  | (PvInh _ _) -> assert False
  ]) ([], 1) l in
  let branch_key_slotnums = List.rev rev_branch_slotnums in
  (branch_key_slotnums, nextslot-1)
;

(** convert a pvariant type into a tuple type.

   By reference to the following type, I'll describe what we need:

   type t = [ `A [@key 1] | `B [@key 2] | `C of int [@key 3] | `D [@key 4] | `E of int * string [@key 5] ]

   Each branch (MUST) has a key.  We must compute a "slot number"
   for each branch in the tuple-type, which will be:

   (int[@key 1] * option int [@key 3+1] * option(int * string) [@key 5+1])

   Branches with arguments get slot-numbers.  All branches get keys (1+ the declared key).

   The first int take the value of the original branch keys.
*)
value to_tupletype loc arg branch_key_slotnums =
  let totuple_branch2tuplety = fun [
    {it=(PvTag loc cid _ <:vala< [] >> attrs)} ->
    []

  | {it=(PvTag loc cid _ tyl attrs);newkey=newkey; arity=None} ->
    let branchtuplety = tuplectyp loc (uv tyl) in
    [<:ctyp< option $branchtuplety$ [@key $int:string_of_int newkey$; ] >>]

  | {it=(PvTag loc cid _ tyl attrs);newkey=newkey; arity=Some _} ->
    let branchtuplety = tuplectyp loc (uv tyl) in
    [<:ctyp< $branchtuplety$ [@key $int:string_of_int newkey$; ] >>]

  | {it=PvInh _ _ } -> assert False
  ] in
    tuplectyp loc [ <:ctyp< int [@ key 1;] >> :: (List.concat (List.map totuple_branch2tuplety branch_key_slotnums)) ]
;

(** convert a variant value into a tuple. *)
value totuple loc arg argvar (branch_key_slotnums, nslots) =

  let all_empty_expr = Variant.all_empty_expr loc branch_key_slotnums in

  let totuple_branch2tuple_expr argvars = fun [
    {it=(PvTag loc cid _ <:vala< [] >> attrs); oldkey=oldkey; slotnum=None} ->
    tupleexpr loc [ <:expr< $int:string_of_int oldkey$ >> :: all_empty_expr ]

  | {it=(PvTag loc cid _ tyl attrs); oldkey=oldkey; slotnum=Some slotnum; arity=arity} ->
    (* (Some i, None ... None ..., Some <tuple-of-argvars>, None ...) *)
    let argtuple_expr =  tupleexpr loc (List.map (fun v -> <:expr< $lid:v$ >>) argvars) in
    let arg_expr = match arity with [
      None -> <:expr< Some $argtuple_expr$ >>
    | Some _ -> argtuple_expr
    ] in
    tupleexpr loc [ <:expr< $int:string_of_int oldkey$ >>
      :: Variant.empty_except_i_expr loc branch_key_slotnums (slotnum, arg_expr) ]

  | {it=PvInh _ _ } -> assert False
  ] in

  let totuple_branch2branch_patt = fun [
    {it=(PvTag loc cid _ <:vala< [] >> attrs); oldkey=oldkey; slotnum=None} ->
    (<:patt< ` $uv cid$ >>, [])

  | {it=(PvTag loc cid _ tyl attrs); oldkey=oldkey; slotnum=Some slotnum} ->
    let argvars = List.mapi (fun i _ -> Printf.sprintf "v_%d" i) (uv tyl) in
    (Patt.applist <:patt< ` $uv cid$ >> (List.map (fun v -> <:patt< $lid:v$ >>) argvars),
     argvars)

  | {it=PvInh _ _ } -> assert False
  ] in

  let totuple_branches = List.map (fun bks ->
    let (patt,argvars) = totuple_branch2branch_patt bks in
    let expr = totuple_branch2tuple_expr argvars bks in
    (patt, <:vala< None >>, expr)) branch_key_slotnums in
  <:expr< match v with [ $list:totuple_branches$ ] >>
;

value oftuple ~{coercion} loc arg ~{field_name} (branch_key_slotnums, nslots) =
  let msg = format_msg arg field_name in
  let all_empty_patt = Variant.all_empty_patt loc branch_key_slotnums in

  let oftuple_branch2tuple_patt argvars = fun [
    {it=(PvTag loc cid _ <:vala< [] >> attrs); oldkey=oldkey; slotnum=None} ->
    tuplepatt loc [ <:patt< $int:string_of_int oldkey$ >> :: all_empty_patt ]

  | {it=(PvTag loc cid _ tyl attrs); oldkey=oldkey; slotnum=Some slotnum; arity=arity} ->
    (* (Some i, None ... None ..., Some <tuple-of-argvars>, None ...) *)
    let argtuple_patt =  tuplepatt loc (List.map (fun v -> <:patt< $lid:v$ >>) argvars) in
    let argtuple_patt =  tuplepatt loc (List.map (fun v -> <:patt< $lid:v$ >>) argvars) in
    let arg_patt = match arity with [
      None -> <:patt< Some $argtuple_patt$ >>
    | Some _ -> argtuple_patt
    ] in
    tuplepatt loc [ <:patt< $int:string_of_int oldkey$ >>
      :: Variant.empty_except_i_patt loc branch_key_slotnums (slotnum, arg_patt) ]

  | {it=PvInh _ _ } -> assert False
  ] in

  let oftuple_branch2branch_expr = fun [
    {it=(PvTag loc cid _ <:vala< [] >> attrs); oldkey=oldkey; slotnum=None} ->
    (<:expr< ` $uv cid$ >>, [])

  | {it=(PvTag loc cid _ tyl attrs);oldkey=oldkey; slotnum=Some slotnum} ->
    let argvars = List.mapi (fun i _ -> Printf.sprintf "v_%d" i) (uv tyl) in
    (Expr.applist <:expr< ` $uv cid$ >> (List.map (fun v -> <:expr< $lid:v$ >>) argvars),
     argvars)

  | {it=PvInh _ _ } -> assert False
  ] in

  let oftuple_branches = List.map (fun bks ->
    let (expr,argvars) = oftuple_branch2branch_expr bks in
    let patt = oftuple_branch2tuple_patt argvars bks in
    (patt, <:vala< None >>, <:expr< ( $expr$ : $coercion$ ) >>)) branch_key_slotnums in
  let oftuple_branches = oftuple_branches @ [
    (<:patt< _ >>, <:vala< None >>,
     <:expr< raise
             (let open Protobuf.Decoder in
                Failure (Malformed_variant $str:msg$)) >>)
  ] in
  <:expr< fun [ $list:oftuple_branches$ ] >>
;

end ;

module To = struct

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< $pty$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >> ; end) ;

value to_protobuf_fname arg tyname =
  tyname^"_to_protobuf"
;

value fmt_attrmod_modifier a =
 let loc = Ploc.dummy in
 match a with [
  {arity=None; default=None} -> <:expr< required >>
| {arity=None; default=Some e} -> <:expr< required_default $e$ >>
| {arity=Some `Optional} -> <:expr< optional >>
| {arity=Some `List} -> <:expr< list >>
| {arity=Some `Array} -> <:expr< array >>
| _ -> assert False
] ;

value prim_encoder loc attrmod fname =
  let encode0 = <:expr< encode0 $lid:fname$ ~{msg=msg} ~{key= $int:fmt_attrmod_key attrmod$ } >> in
  let wrapped = match attrmod with [
    {arity=None; default=None} -> <:expr< required $encode0$ >>
  | {arity=None; default=Some e} -> <:expr< required_default $e$ $encode0$ >>
  | {default=Some _} -> Ploc.raise loc (Failure "can only specify default for required fields")
  | {arity=Some `Optional} -> <:expr< optional $encode0$ >>

  | {arity=Some `List; packed=True} ->
    <:expr< list_encode_packed ~{key= $int:fmt_attrmod_key attrmod$ } ~{msg=msg} $lid:fname$ >>

  | {arity=Some `Array; packed=True} ->
    <:expr< array_encode_packed ~{key= $int:fmt_attrmod_key attrmod$ } ~{msg=msg} $lid:fname$ >>

  | {arity=Some `List} -> <:expr< list $encode0$ >>
  | {arity=Some `Array} -> <:expr< array $encode0$ >>
  | _ -> assert False
  ] in
  <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $wrapped$ >>
;

value bare_to_expression arg ~{field_name} ty0 =
  let msg = format_msg arg field_name in
  match ty0 with [
    <:ctyp:< [ $list:l$ ] >> ->
    let branches = List.map (fun [ (_, cid, <:vala< [] >>, _, attrs) ->
      let key = attrs_to_key loc arg attrs in
      (<:patt< $_uid:cid$ >>, <:vala< None >>, <:expr< $int64:string_of_int key$ >>)
    | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to ENUM [p]variants")
    ]) l in
    <:expr<fun [ $list:branches$ ] >>

  | <:ctyp:< [= $list:l$ ] >> ->
    let branches = List.map (fun [ PvTag _ cid _ <:vala< [] >> attrs ->
      let key = attrs_to_key loc arg attrs in
      (<:patt< ` $uv cid$ >>, <:vala< None >>, <:expr< $int64:string_of_int key$ >>)
    | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to CLOSED ENUM [p]variants")
    ]) l in
    <:expr<fun [ $list:branches$ ] >>

  | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to enum [p]variants")
  ]
;

value to_expression arg ?{coercion} ~{field_name} param_map ty0 =
  let runtime_module =
    let loc = loc_of_ctyp ty0 in
    Base.expr_runtime_module <:expr< Runtime >> in
  let rec fmtrec ?{coercion} ~{attrmod} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod.bare ->
  let fname = to_protobuf_fname arg lid in
  let bare_fname = fname^"_bare" in
  let f = prim_encoder loc attrmod "int64__varint" in
  (attrmod, <:expr< fun v encoder -> $f$ ($lid:bare_fname$ v) encoder >>)

(*
| <:ctyp:< _ >> -> failwith "cannot derive yojson for type <<_>>"
| <:ctyp:< Yojson.Safe.t >> -> <:expr< fun x -> x >>
| <:ctyp:< unit >> -> <:expr< $runtime_module$.Yojson.unit_to_yojson >>
*)
| <:ctyp:< int >> when attrmod.encoding = Some Zigzag ->
  (attrmod, prim_encoder loc attrmod "int__zigzag")

| <:ctyp:< int >> when attrmod.encoding = Some Bits32 ->
  (attrmod, prim_encoder loc attrmod "int__bits32")

| <:ctyp:< int >> when attrmod.encoding = Some Bits64 ->
  (attrmod, prim_encoder loc attrmod "int__bits64")

| <:ctyp:< int >> ->
  (attrmod, prim_encoder loc attrmod "int__varint")
| <:ctyp:< bool >> ->
  (attrmod, prim_encoder loc attrmod "bool__varint")

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, prim_encoder loc attrmod "int32__bits32")

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, prim_encoder loc attrmod "int32__bits64")

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, prim_encoder loc attrmod "int32__varint")

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, prim_encoder loc attrmod "int32__zigzag")

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, prim_encoder loc attrmod "uint32__bits32")

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, prim_encoder loc attrmod "uint32__bits64")

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, prim_encoder loc attrmod "uint32__varint")

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, prim_encoder loc attrmod "uint32__zigzag")

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, prim_encoder loc attrmod "int64__bits64")

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, prim_encoder loc attrmod "int64__bits32")

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, prim_encoder loc attrmod "int64__varint")

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, prim_encoder loc attrmod "int64__zigzag")

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, prim_encoder loc attrmod "uint64__bits64")

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, prim_encoder loc attrmod "uint64__bits32")

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, prim_encoder loc attrmod "uint64__varint")

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, prim_encoder loc attrmod "uint64__zigzag")

| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) -> do {
    if attrmod.packed then Ploc.raise loc (Failure "bytes fields cannot be packed") else () ;
    (attrmod, prim_encoder loc attrmod "string__bytes")
  }

| (<:ctyp:< bytes >> | <:ctyp:< Stdlib.Bytes.t >> | <:ctyp:< Bytes.t >>) -> do {
    if attrmod.packed then Ploc.raise loc (Failure "bytes fields cannot be packed") else () ;
    (attrmod, prim_encoder loc attrmod "bytes__bytes")
  }

(*
| <:ctyp:< char >> -> <:expr< fun x -> $runtime_module$.Yojson.string_to_yojson (String.make 1 x) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< $runtime_module$.Yojson.nativeint_to_yojson >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> ->
    <:expr< fun x -> $runtime_module$.Yojson.string_to_yojson (Nativeint.to_string x) >>
*)
| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = None || attrmod.encoding = Some Bits64 ->
  (attrmod, prim_encoder loc attrmod "float__bits64")

| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, prim_encoder loc attrmod "float__bits32")

(*
| <:ctyp:< Hashtbl.t >> ->
  <:expr< $runtime_module$.Yojson.hashtbl_to_yojson >>
*)
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "encoding" ->
    let encoding = match e with [
      <:expr< `varint >> -> Varint
    | <:expr< `zigzag >> -> Zigzag
    | <:expr< `bits32 >> -> Bits32
    | <:expr< `bits64 >> -> Bits64
    ] in
    fmtrec ~{attrmod={ (attrmod) with encoding = Some encoding } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $int:key$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "key" ->
    let key = int_of_string key in
    fmtrec ~{attrmod={ (attrmod) with key = Some key } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:dflt$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "default" ->
    fmtrec ~{attrmod={ (attrmod) with default = Some dflt } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "bare" ->
    fmtrec ~{attrmod={ (attrmod) with bare = True } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $str:fld$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "fieldname" ->
    fmtrec ~{attrmod={ (attrmod) with field_name = fld } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "packed" ->
    fmtrec ~{attrmod={ (attrmod) with packed = True } } t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } ty

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `List ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< listmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (List.map (fun v -> { it_list = v }) v) encoder >>)

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `Array ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< listmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (Array.map (fun v -> { it_list = v }) v) encoder >>)

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< listmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (Option.map (fun v -> { it_list = v }) v) encoder >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } ty

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `Array ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< arraymsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (Array.map (fun v -> { it_array = v }) v) encoder >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< arraymsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (Option.map (fun v -> { it_array = v }) v) encoder >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `List ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< arraymsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (List.map (fun v -> { it_array = v }) v) encoder >>)

(*
| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< $runtime_module$.Yojson.ref_to_yojson $fmt1$ >>
*)
| <:ctyp:< option $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } ty

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< optionmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (match v with [ None -> None | Some v -> Some { it_option = v } ]) encoder >>)

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `List ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< optionmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (List.map (fun v -> { it_option = v }) v) encoder >>)

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `Array ->
  let (am, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< optionmsg $ty$ >> in
  (am,
   <:expr< fun v encoder -> $fmt$ (Array.map (fun v -> { it_option = v }) v) encoder >>)

| <:ctyp:< $_$ $_$ >> as ty ->
  let (tyf, tyargs) = Ctyp.unapplist ty in
  let lid = match tyf with [
    <:ctyp:< $lid:lid$ >> -> lid
  | _ -> Ploc.raise loc (Failure "type-application with lhs not a type-name")
  ] in
  let fmtargs = List.map (fun ty -> let (_, e) = fmtrec ~{attrmod= { (init_attrmod attrmod.field_name) with param = True } } ty in e) tyargs in
  let fname = to_protobuf_fname arg lid in
  let f = Expr.applist <:expr< $lid:fname$ >> fmtargs in
  (attrmod,
   if attrmod.param then f else
   <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (fun v encoder -> do {
            Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder ;
            Protobuf.Encoder.nested ($f$ v) encoder
          }) >>)

| <:ctyp:< $lid:lid$ >> ->
  let fname = to_protobuf_fname arg lid in
  (attrmod,
   if attrmod.param then <:expr< $lid:fname$ >> else
   <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (fun v encoder -> do {
            Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder ;
            Protobuf.Encoder.nested ($lid:fname$ v) encoder
          }) >>)

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.protobuf: unrecognized param-var in type-decl"
  ] in
  let e = PM.arg_expr loc p in
  (attrmod,
   if attrmod.param then e else
   <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (fun v encoder -> do {
            Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder ;
            Protobuf.Encoder.nested ($e$ v) encoder
          }) >>)

(*
| <:ctyp:< $lid:lid$ >> ->
  let fname = to_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = to_protobuf_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>
*)

| <:ctyp:< [ $list:l$ ] >> -> do {
(* (1) convert branches into a tuple-type
   (2) generate encoder
   (3) convert variant to tuple
   (4) and apply encoder
   N.B. since this is a variant, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let (branch_key_slotnums, nslots) = Variant.preprocess loc arg attrmod l in
  let tuplety = Variant.to_tupletype loc arg branch_key_slotnums in
  let to_tuple_expr = Variant.totuple loc arg "v" (branch_key_slotnums, nslots) in
  let (_, fmt) = fmtrec ~{attrmod=attrmod} tuplety in
  (attrmod, <:expr< fun v encoder -> $fmt$ $to_tuple_expr$ encoder >>)
  }

| <:ctyp:< [= $list:l$ ] >> as ty0 when attrmod.bare ->
  let f = bare_to_expression arg ~{field_name=attrmod.field_name} ty0 in
  let enc = prim_encoder loc attrmod "int64__varint" in
  (attrmod, <:expr< fun v encoder -> $enc$ ($f$ v) encoder >>)

| <:ctyp:< [= $list:l$ ] >> ->
    let (branch_key_slotnums, nslots) = PVariant.preprocess loc arg attrmod l in
    let tuplety = PVariant.to_tupletype loc arg branch_key_slotnums in
    let to_tuple_expr = PVariant.totuple loc arg "v" (branch_key_slotnums, nslots) in
    let (_, fmt) =
      let attrmod = { (init_attrmod attrmod.field_name) with message = attrmod.message } in
      fmtrec ~{attrmod=attrmod} tuplety in
    let e = <:expr< fun v encoder -> $fmt$ $to_tuple_expr$ encoder >> in
    let e = if not attrmod.message then
      <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (fun v encoder -> do {
                Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder;
                Protobuf.Encoder.nested ($e$ v) encoder }) >>
      else e in
    (attrmod, e)

| <:ctyp:< ( $list:tyl$ ) >> ->
    let am_fmt_vars = List.mapi (fun i ty ->
      let fld = Printf.sprintf "%s.%d" attrmod.field_name i in
      let attrmod = { (init_attrmod fld) with key = Some (i+1) } in
      (fmtrec ~{attrmod=attrmod} ty, Printf.sprintf "v%d" i))
    tyl in
    (* ordered by occurrence in tuple-type *)
    let vars = List.map snd am_fmt_vars in
    (* ordered by key value *)
    let am_fmt_vars = List.sort (fun ((a, _),_) ((b, _),_) -> attrmod_key_compare a b) am_fmt_vars in
    let exps = List.map (fun ((_, fmt), v) -> <:expr< $fmt$ $lid:v$ encoder >>) am_fmt_vars in

    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let tuple_varpats = tuplepatt loc varpats in
    let e = <:expr< fun $tuple_varpats$ encoder -> do { $list:exps$ } >> in
    let e = if not attrmod.message then
      <:expr< let open Pa_ppx_protobuf.Runtime.Encode in
    $fmt_attrmod_modifier attrmod$
    (fun v encoder -> do {
                Protobuf.Encoder.key ($int:fmt_attrmod_key attrmod$, Protobuf.Bytes) encoder;
                Protobuf.Encoder.nested ($e$ v) encoder }) >>
      else e in
  (attrmod, e)

| <:ctyp:< { $list:fields$ } >> -> do {
(* (1) convert fields into a tuple-type
   (2) generate encoder
   (3) convert record to tuple
   (4) and apply encoder
   N.B. since this is a record, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let tupty =
    let l = List.map (fun (_, fld, _, ty, attrs) ->
        let fld = attrmod.field_name^"."^fld in
        Ctyp.wrap_attrs <:ctyp< $ty$ [@fieldname $str:fld$ ;] >> (uv attrs)
     ) fields in
    <:ctyp< ( $list:l$ ) >> in
  let (_, e) = fmtrec ~{attrmod=attrmod} tupty in
  let fields_as_tuple =
    let l = List.map (fun (_, n, _, _, _) ->
      <:expr< v . $lid:n$ >>
    ) fields in
    tupleexpr loc l in
  (attrmod, <:expr< fun v encoder -> $e$ $fields_as_tuple$ encoder >>)

  }

| [%unmatched_vala] -> failwith "pa_deriving_protobuf.to_expression"
]
in
let (am, e) = fmtrec ?{coercion=coercion} ~{attrmod={ (init_attrmod field_name) with message = True } } ty0 in
let loc = loc_of_expr e in
(am, <:expr< let msg = $str:format_msg arg field_name$ in $e$ >>)
;

value fmt_to_top arg ~{coercion} ~{field_name} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  snd (to_expression arg ~{coercion=coercion} ~{field_name=field_name} params t2)
| t -> snd (to_expression arg ~{coercion=coercion} ~{field_name=field_name} params t)
]
;

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let to_protobuffname = to_protobuf_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Pa_ppx_protobuf.Runtime.Protobuf.Encoder.t -> unit >> in
  (to_protobuffname, toftype)
;

value sig_item_funs arg td =
  [sig_item_fun0 arg td]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (to_protobuffname, toftype) = sig_item_fun0 arg td in
  let loc = loc_of_type_decl td in
  [ <:sig_item< value $lid:to_protobuffname$ : $toftype$>> ]
}
;

value bare_str_items arg td =
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let tyname = uv tyname in
  if not (Ctxt.is_bare arg tyname) then [] else do {
  assert ([] = uv td.tdPrm) ;
  let tk = td.tdDef in
  let to_protobuffname = to_protobuf_fname arg tyname in
  let bare_name = to_protobuffname^"_bare" in
  let to_e = bare_to_expression arg ~{field_name=tyname} tk in
  [(<:patt< $lid:bare_name$ >>, to_e, <:vala< [] >>)]
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tk = td.tdDef in
  let tyname = uv tyname in
  let ty =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    (Ctyp.applist ty paramtys) in
  let coercion =
    monomorphize_ctyp ty in
  let to_protobuffname = to_protobuf_fname arg tyname in
  let to_e = fmt_to_top arg ~{coercion=coercion} ~{field_name=tyname} param_map tk in
  let to_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let (_, fty) = sig_item_fun0 arg td in
  let fty = PM.quantify_over_ctyp param_map fty in
  let argexp =
    if uv td.tdPrv && is_type_abbreviation tk then
      <:expr< ( arg : $monomorphize_ctyp ty$ :> $monomorphize_ctyp tk$ ) >>
    else <:expr< arg >> in
  [(<:patt< ( $lid:to_protobuffname$ : $fty$ ) >>,
    Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg encoder -> $to_e$ $argexp$ encoder >>, <:vala< [] >>)]
  @ (bare_str_items arg td)
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let (to_protobuffname, toftype) = sig_item_fun0 arg td in
    let sil = [<:sig_item< value $lid:to_protobuffname$ : $toftype$>>] in
    let modname = Printf.sprintf "M_%s" to_protobuffname in
    let field_type = PM.quantify_over_ctyp param_map toftype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:to_protobuffname$ = { f: mutable  $field_type$ } ;
      value f : $lid:to_protobuffname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let (to_protobuffname, toftype) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" to_protobuffname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving protobuf] is missing when extending the type " to_protobuffname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map toftype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:to_protobuffname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:to_protobuffname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (to_protobuf_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "protobuf" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = snd(to_expression arg ~{field_name=String.escaped (Pp_MLast.show_longid_lident t)} param_map ty) in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps@[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

module Of = struct

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< Pa_ppx_protobuf.Runtime.Protobuf.Decoder.t -> $pty$ >> ; end) ;

value of_protobuf_fname arg tyname =
  tyname^"_from_protobuf"
;

(**
EXAMPLE (type i1 = int [@key 1])
let i1_from_protobuf_manually decoder =
  let v0 = None in
  let update_v0 v0 (newv : int) = Some newv in
  let rec derec v0 = match Protobuf.Decoder.key decoder with
    | Some (1, kind) ->
      let v0 = update_v0 v0 
      (let open Pa_ppx_protobuf.Runtime.Decode in
            ((decode0 int__varint ~msg:"Test_syntax.i1" kind) decoder))
       in
      derec v0
    (* IF the field is packed+repeated (implies it's primitive, and NOT bytes) *)
    | (Some 1, Protobuf.Bytes) ->
      let v0 = list_packed_rev_append (decode0 int__varint ~msg:"Test_syntax.i1") decoder v0 in
      derec v0

    | Some (_, kind) -> ( Protobuf.Decoder.skip decoder kind ; derec v0 )
    | None -> v0
  in match derec v0 with
    Some v0 -> v0
  | _ ->
    (* a check for each field *)
    if None = v0 then
      raise
        (let open Protobuf.Decoder in
            Failure (Missing_field "Test_syntax.i1"))
    else () ;
    (* and then a catch-all which should never be triggered *)
      raise
        (let open Protobuf.Decoder in
            Failure (Missing_field "Test_syntax.i1"))

GENERALIZE
let i1_from_protobuf_manually = (*e1*) fun decoder ->
  (*e2*)<initialize vars v_i for each type with either None or the empty value of the type (for optional, list, array)>
  (*e3*)<define updaters for each type>
  (*e4*)let rec derec (<tuple-of-vars>) = match Protobuf.Decoder.key decoder with
    (*BEGIN branch5*)| Some (<key_i>, kind) ->
      (*e5*)let v_i = update_v_i v_i
      (let open Pa_ppx_protobuf.Runtime.Decode in
            ((<decoder-expression for type> ~msg:"Test_syntax.i1" kind) decoder))
       in
      derec <tuple-of-vars>

    | Some(<key_i>, Protobuf.Bytes) ->
      (*e5b*)let open Pa_ppx_protobuf.Runtime.Decode in
      let v_i = list_packed_rev_append (<decoder-expression for type> ~msg:"Test_syntax.i1") decoder v_i in
      derec <tuple-of-vars>
    (*END branch5*)

    | Some (_, kind) -> ( Protobuf.Decoder.skip decoder kind ; derec <tuple-of-vars> )
    | None -> <tuple-of-vars>
  in (*e6*)match derec <tuple-of-vars> with
    <final-value-checks-projections-for-each-type>  -> <final-value-for-each-type>
  | _ -> do {
    <final-value-errors> ;
    raise
        (let open Protobuf.Decoder in
            Failure (Missing_field "Test_syntax.i1"))
    }
*)
value demarshal_to_tuple loc arg am_kind_fmt_vars =
  let initvals = List.map (fun (am, kind, fmt, v) ->
    let e = match am with [
      {arity=None; default=Some e} -> <:expr< Some $e$ >>
    | {arity=Some _; default=Some _} -> Ploc.raise loc (Failure "can only  specify default for required field")
    | {arity=(None | Some `Optional)} -> <:expr< None >>
    | {arity=(Some (`List | `Array))}  -> <:expr< [] >>
    ] in
    (<:patt< $lid:v$ >>, e, <:vala< [] >>)) am_kind_fmt_vars in
  let updaters = List.map (fun (am, _, _, v) ->
    let fname = Printf.sprintf "update_%s" v in
    let e = match am.arity with [
      None -> <:expr< Some newv >>
    | Some `Optional -> <:expr< Some newv >>
    | Some ( `List | `Array ) -> <:expr< [ newv :: $lid:v$ ] >>
    ] in
    (<:patt< $lid:fname$ >>,
     <:expr< fun $lid:v$ newv -> $e$ >>, 
     <:vala< [] >>)) am_kind_fmt_vars in
  let tuple_of_vars_patt =
    let l = List.map (fun (_, _, _, v) -> <:patt< $lid:v$ >>) am_kind_fmt_vars in
    tuplepatt loc l in
  let tuple_of_vars_expr =
    let l = List.map (fun (_, _, _, v) -> <:expr< $lid:v$ >>) am_kind_fmt_vars in
    tupleexpr loc l in
  let final_value_patts = List.map (fun (am, _, _, v) ->
    match am.arity with [
      None -> <:patt< Some $lid:v$ >>
    | Some ( `List | `Array | `Optional ) -> <:patt< $lid:v$ >>
    ]) am_kind_fmt_vars in
  let final_value_errors = List.concat (List.map (fun (am, _, _, v) ->
    match am.arity with [
      None -> [<:expr< if None = $lid:v$ then
        raise (let open Protobuf.Decoder in Failure (Missing_field $str:format_msg arg am.field_name$))
      else ()
      >>]
    | Some ( `List | `Array | `Optional ) -> []
    ]) (List.rev am_kind_fmt_vars)) in
  let final_value_errors = final_value_errors @ [
    <:expr< raise (let open Protobuf.Decoder in Failure (Missing_field msg)) >>
  ] in
  let final_value_patt = tuplepatt loc final_value_patts in
  let final_value_exprs = List.map (fun (am, _, _, v) ->
    match am.arity with [
      None -> <:expr< $lid:v$ >>
    | Some `List -> <:expr< List.rev $lid:v$ >>
    | Some `Array -> <:expr< Array.of_list (List.rev $lid:v$) >>
    | Some `Optional -> <:expr< $lid:v$ >>
    ]) am_kind_fmt_vars in
  let final_value_expr = tupleexpr loc final_value_exprs in

  let e6 = <:expr< match derec $tuple_of_vars_expr$ with [
     $final_value_patt$ -> $final_value_expr$
   | _ -> do { $list:final_value_errors$ } ] >> in

  let branch5s = List.map (fun (am, kind, fmt, v) ->
    let updatename = Printf.sprintf "update_%s" v in
    let e5 =
      <:expr< let $lid:v$ = $lid:updatename$ $lid:v$
       (let open Pa_ppx_protobuf.Runtime.Decode in
         $fmt$ decoder)
         in derec $tuple_of_vars_expr$ >> in
    let branch5_e5 = [(<:patt< Some ($int:fmt_attrmod_key am$, $kind$) >>, <:vala< None >>, e5)] in
    let e5b =
      <:expr< let open Pa_ppx_protobuf.Runtime.Decode in
        let $lid:v$ = list_packed_rev_append $fmt$ decoder $lid:v$
         in derec $tuple_of_vars_expr$ >> in
    let branch5_e5b = if am.arity = Some `List ||  am.arity = Some `Array then
        [(<:patt< Some ($int:fmt_attrmod_key am$, Protobuf.Bytes) >>, <:vala< None >>, e5b)]
      else [] in
    let unexpected = <:expr< raise (let open Protobuf.Decoder in Failure (Unexpected_payload $str:format_msg arg am.field_name$ kind)) >> in
    let branch5_unexpected = [(<:patt< Some ($int:fmt_attrmod_key am$, kind) >>, <:vala< None >>, unexpected)] in
    branch5_e5 @ branch5_e5b @ branch5_unexpected
  ) am_kind_fmt_vars in
  let branch5s = List.concat branch5s in

  let fallback_branches = [
    (<:patt< Some (_, kind) >>, <:vala< None >>,
     <:expr< do { Protobuf.Decoder.skip decoder kind ; derec $tuple_of_vars_expr$ } >>) ;
    (<:patt< None >>, <:vala< None >>, tuple_of_vars_expr)
  ] in

  let e4 = <:expr< let rec derec $tuple_of_vars_patt$ = match Protobuf.Decoder.key decoder with
                   [ $list:branch5s@fallback_branches$ ]
                   in $e6$ >> in

  let e3 = <:expr< let $list:updaters$ in $e4$ >> in
  let e2 = <:expr< let $list:initvals$ in $e3$ >> in
  <:expr< fun decoder -> $e2$ >>
;

value bare_of_expression arg ~{field_name} ty0 =
  let msg = format_msg arg field_name in
  match ty0 with [
    <:ctyp:< [ $list:l$ ] >> ->
    let branches = List.map (fun [ (_, cid, <:vala< [] >>, _, attrs) ->
      let key = attrs_to_key loc arg attrs in
      (<:patt< $int64:string_of_int key$ >>, <:vala< None >>, <:expr< $_uid:cid$ >>)
    | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to ENUM [p]variants")
    ]) l in
    let branches = branches @ [
      (<:patt< _ >>, <:vala< None >>,
       <:expr< raise (let open Protobuf.Decoder in
               Failure (Malformed_variant $str:msg$)) >>)
    ] in
    <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< [= $list:l$ ] >> ->
    let branches = List.map (fun [ PvTag _ cid _ <:vala< [] >> attrs ->
      let key = attrs_to_key loc arg attrs in
      (<:patt< $int64:string_of_int key$ >>, <:vala< None >>, <:expr< ` $uv cid$ >>)
    | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to CLOSED ENUM [p]variants")
    ]) l in
    let branches = branches @ [
      (<:patt< _ >>, <:vala< None >>,
       <:expr< raise (let open Protobuf.Decoder in
               Failure (Malformed_variant $str:msg$)) >>)
    ] in
    <:expr< fun [ $list:branches$ ] >>

  | _ -> Ploc.raise (loc_of_ctyp ty0) (Failure "protobuf.bare: only applicable to enum [p]variants")
  ]
;

value trace_before_fmtrec (attrmod : attrmod_t) (ty : MLast.ctyp) = () ;
value trace_after_fmtrec (attrmod : attrmod_t) (ty : MLast.ctyp) (rv : (attrmod_t * MLast.patt * MLast.expr)) = () ;

value of_expression arg ~{attrmod} param_map ty0 =
  let runtime_module =
    let loc = loc_of_ctyp ty0 in
    Base.expr_runtime_module <:expr< Runtime >> in
  let rec fmtrec ~{attrmod} ty = do {
    trace_before_fmtrec attrmod ty ;
    let rv = obs_fmtrec ~{attrmod=attrmod} ty in do {
      trace_after_fmtrec attrmod ty rv ;
      rv
    }
  }

  and obs_fmtrec ~{attrmod} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod.bare ->
  let fname = of_protobuf_fname arg lid in
  let bare_fname = fname^"_bare" in
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< let open Pa_ppx_protobuf.Runtime.Decode in
          decode0 { kind = Protobuf.Varint ; convertf = Pa_ppx_protobuf.Runtime.forget1 $lid:bare_fname$ ; decodef = Protobuf.Decoder.varint } ~{msg=msg} >>)

(*
| <:ctyp:< Protobuf.Safe.t >> -> <:expr< fun x -> Result.Ok x >>
| <:ctyp:< unit >> -> <:expr< $runtime_module$.Protobuf.unit_of_protobuf $str:msg$ >>
*)
| <:ctyp:< int >> when attrmod.encoding = Some Zigzag -> 
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 int__zigzag ~{msg=msg}) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits32 -> 
  (attrmod, <:patt< Protobuf.Bits32 >>,
  <:expr< (decode0 int__bits32 ~{msg=msg}) >>)

| <:ctyp:< int >> when attrmod.encoding = Some Bits64 -> 
  (attrmod, <:patt< Protobuf.Bits64 >>,
  <:expr< (decode0 int__bits64 ~{msg=msg}) >>)

| <:ctyp:< int >> -> 
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 int__varint ~{msg=msg})>>)

| <:ctyp:< bool >> ->
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< (decode0 bool__variant ~{msg=msg}) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 int32__bits32 ~{msg=msg}) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 int32__bits64 ~{msg=msg}) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int32__varint ~{msg=msg}) >>)

| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int32__zigzag ~{msg=msg}) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits32 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 uint32__bits32 ~{msg=msg}) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 uint32__bits64 ~{msg=msg}) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint32__varint ~{msg=msg}) >>)

| <:ctyp:< uint32 >> | <:ctyp:< Uint32.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint32__zigzag ~{msg=msg}) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 int64__bits64 ~{msg=msg}) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 int64__bits32 ~{msg=msg}) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int64__varint ~{msg=msg}) >>)

| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 int64__zigzag ~{msg=msg}) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits64 || attrmod.encoding = None ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 uint64__bits64 ~{msg=msg}) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 uint64__bits32 ~{msg=msg}) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Varint ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint64__varint ~{msg=msg}) >>)

| <:ctyp:< uint64 >> | <:ctyp:< Uint64.t >> when attrmod.encoding = Some Zigzag ->
  (attrmod, <:patt< Protobuf.Varint >>,
 <:expr< (decode0 uint64__zigzag ~{msg=msg}) >>)

| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) -> do {
    if attrmod.packed then Ploc.raise loc (Failure "bytes fields cannot be packed") else () ;
    (attrmod, <:patt< Protobuf.Bytes >>,
     <:expr< (decode0 string__bytes ~{msg=msg}) >>)
  }

| (<:ctyp:< bytes >> | <:ctyp:< Stdlib.Bytes.t >> | <:ctyp:< Bytes.t >>) -> do {
    if attrmod.packed then Ploc.raise loc (Failure "bytes fields cannot be packed") else () ;
    (attrmod, <:patt< Protobuf.Bytes >>,
     <:expr< (decode0 bytes__bytes ~{msg=msg}) >>)
  }
(*
| <:ctyp:< char >> -> <:expr< fun [ `String x ->
          if (String.length x) = 1
          then Result.Ok (x.[0])
          else Result.Error $str:msg$
      | _ -> Result.Error $str:msg$ ] >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< $runtime_module$.Protobuf.nativeint_of_protobuf $str:msg$ >>
| <:ctyp:< nativeint [@encoding `string ; ] >> | <:ctyp:< Nativeint.t [@encoding `string ; ] >> -> <:expr< fun [
        `String x -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error $str:msg$ ] >>
*)
| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = None || attrmod.encoding = Some Bits64 ->
  (attrmod, <:patt< Protobuf.Bits64 >>,
 <:expr< (decode0 float__bits64 ~{msg=msg}) >>)

| <:ctyp:< float >> | <:ctyp:< Float.t >> when attrmod.encoding = Some Bits32 ->
  (attrmod, <:patt< Protobuf.Bits32 >>,
 <:expr< (decode0 float__bits32 ~{msg=msg}) >>)

(*
| <:ctyp:< Hashtbl.t >> ->
  <:expr< $runtime_module$.Protobuf.hashtbl_of_protobuf >>
*)
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "encoding" ->
    let encoding = match e with [
      <:expr< `varint >> -> Varint
    | <:expr< `zigzag >> -> Zigzag
    | <:expr< `bits32 >> -> Bits32
    | <:expr< `bits64 >> -> Bits64
    ] in
    fmtrec ~{attrmod= { (attrmod) with encoding = Some encoding } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $int:key$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "key" ->
    let key = int_of_string key in
    fmtrec ~{attrmod={ (attrmod) with key = Some key } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:dflt$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "default" ->
    fmtrec ~{attrmod={ (attrmod) with default = Some dflt } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "bare" ->
    fmtrec ~{attrmod={ (attrmod) with bare = True } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $str:fld$; ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "fieldname" ->
    fmtrec ~{attrmod={ (attrmod) with field_name = fld } } t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "protobuf" "packed" ->
    fmtrec ~{attrmod={ (attrmod) with packed = True } } t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } ty

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `List ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< listmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_list=v} -> v ] >>)

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `Array ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< listmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_list=v} -> v ] >>)

| <:ctyp:< list $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< listmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_list=v} -> v ] >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } ty

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `Array ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< arraymsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_array=v} -> v ] >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< arraymsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_array=v} -> v ] >>)

| <:ctyp:< array $ty$ >> when attrmod.arity = Some `List ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< arraymsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ {it_array=v} -> v ] >>)


(*
| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< $runtime_module$.Protobuf.ref_of_protobuf $fmt1$ >>
*)
| <:ctyp:< option $ty$ >> when attrmod.arity = None ->
  fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } ty

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `Optional ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Optional } } <:ctyp< optionmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ { it_option = it } -> it ] >>)

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `List ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `List } } <:ctyp< optionmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ { it_option = it } -> it ] >>)

| <:ctyp:< option $ty$ >> when attrmod.arity = Some `Array ->
  let (am, kind, fmt) = fmtrec ~{attrmod = { (attrmod) with arity = Some `Array } } <:ctyp< optionmsg $ty$ >> in
  (am, kind,
   <:expr< fun decoder -> match $fmt$ decoder with [ { it_option = it } -> it ] >>)

| <:ctyp:< $_$ $_$ >> as ty ->
  let (tyf, tyargs) = Ctyp.unapplist ty in
  let lid = match tyf with [
    <:ctyp:< $lid:lid$ >> -> lid
  | _ -> Ploc.raise loc (Failure "type-application with lhs not a type-name")
  ] in
  let fmtargs = List.map (fun ty -> let (_, _, e) = fmtrec ~{attrmod={ (init_attrmod attrmod.field_name) with param = True } } ty in e) tyargs in
  let fname = of_protobuf_fname arg lid in
  let f = Expr.applist <:expr< $lid:fname$ >> fmtargs in
  (attrmod, <:patt< Protobuf.Bytes >>,
  if attrmod.param then f else
  <:expr< fun decoder -> $f$ (Protobuf.Decoder.nested decoder) >>)

| <:ctyp:< $lid:lid$ >> ->
  let fname = of_protobuf_fname arg lid in
  (attrmod, <:patt< Protobuf.Bytes >>,
  if attrmod.param then <:expr< $lid:fname$ >> else
  <:expr< fun decoder -> $lid:fname$ (Protobuf.Decoder.nested decoder) >>)

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.protobuf: unrecognized param-var in type-decl"
  ] in
  let e = PM.arg_expr loc p in
  (attrmod, <:patt< Protobuf.Bytes >>,
   if attrmod.param then e else
   <:expr< fun decoder -> $e$ (Protobuf.Decoder.nested decoder) >>)

(*
| <:ctyp:< $lid:lid$ >> ->
  let fname = of_protobuf_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = of_protobuf_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>
*)

| <:ctyp:< [ $list:l$ ] >> -> do {
(* (1) convert branches into a tuple-type
   (2) generate decoder
   (3) apply decoder
   (4) and convert tuple to variant
   N.B. since this is a variant, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let (branch_key_slotnums, nslots) = Variant.preprocess loc arg attrmod l in
  let tuplety = Variant.to_tupletype loc arg branch_key_slotnums in
  let of_tuple_expr = Variant.oftuple loc arg ~{field_name=attrmod.field_name} (branch_key_slotnums, nslots) in
  let (_, _, fmt) = fmtrec ~{attrmod=attrmod} tuplety in
  (attrmod, <:patt< Protobuf.Bytes >>,
   <:expr< (fun decoder -> $of_tuple_expr$ ($fmt$ decoder)) >>)
  }

| <:ctyp:< [= $list:l$ ] >> as ty0 when attrmod.bare ->
  let f = bare_of_expression arg ~{field_name=attrmod.field_name} ty0 in
  (attrmod, <:patt< Protobuf.Varint >>,
  <:expr< let open Pa_ppx_protobuf.Runtime.Decode in
          decode0 { kind = Protobuf.Varint ; convertf = Pa_ppx_protobuf.Runtime.forget1 $f$ ; decodef = Protobuf.Decoder.varint } ~{msg=msg} >>)

| <:ctyp:< [= $list:l$ ] >> as ty0 -> 
  let ty0 = monomorphize_ctyp ty0 in
  let (branch_key_slotnums, nslots) = PVariant.preprocess loc arg attrmod l in
  let tuplety = PVariant.to_tupletype loc arg branch_key_slotnums in
  let of_tuple_expr = PVariant.oftuple ~{coercion=ty0} loc arg ~{field_name=attrmod.field_name} (branch_key_slotnums, nslots) in
  let (am, kind, fmt) =
    let attrmod = { (init_attrmod attrmod.field_name) with message = attrmod.message } in
    fmtrec ~{attrmod=attrmod} tuplety in
  let e = <:expr< (fun decoder -> $of_tuple_expr$ ($fmt$ decoder)) >> in
  let e = if not attrmod.message then
    let e = demarshal_to_tuple loc arg [(am, kind, e, "v")] in
    <:expr< fun decoder -> $e$ (Protobuf.Decoder.nested decoder) >>
  else e in
  (attrmod, <:patt< Protobuf.Bytes >>,
   e)


| <:ctyp:< ( $list:tyl$ ) >> ->
    let am_kind_fmt_vars = List.mapi (fun i ty ->
      let fld = Printf.sprintf "%s.%d" attrmod.field_name i in
      let attrmod = { (init_attrmod fld) with key = Some (i+1) } in
      let (am, kind, fmt) = fmtrec ~{attrmod=attrmod} ty in
      (am, kind, fmt, Printf.sprintf "v_%d" i)) tyl in
    let e = demarshal_to_tuple loc arg am_kind_fmt_vars in
    let e = if not attrmod.message then
      <:expr< fun decoder -> $e$ (Protobuf.Decoder.nested decoder) >>
    else e in
    (attrmod, <:patt< Protobuf.Bytes >>, e)

| <:ctyp:< { $list:fields$ } >> -> do {
(* (1) convert fields into a tuple-type
   (2) generate decoder
   (3) apply encoder
   (4) and convert tuple to record
   N.B. since this is a record, it MUST be at top-level (attrmod.message=True).
 *)
  assert attrmod.message ;
  assert (None = attrmod.key) ;
  let tupty =
    let l = List.map (fun (_, fld, _, ty, attrs) ->
        let fld = attrmod.field_name^"."^fld in
        Ctyp.wrap_attrs <:ctyp< $ty$ [@fieldname $str:fld$ ;] >> (uv attrs)
      ) fields in
    <:ctyp< ( $list:l$ ) >> in
  let (_, _, e) = fmtrec ~{attrmod=attrmod} tupty in
  let fields_as_tuple_patt =
    let l = List.map (fun (_, n, _, _, _) ->
      <:patt< $lid:n$ >>
    ) fields in
    tuplepatt loc l in
  let recexpr =
    let l = List.map (fun (_, n, _, _, _) ->
      (<:patt< $lid:n$ >>, <:expr< $lid:n$ >>)
    ) fields in
    <:expr< { $list:l$ } >> in
  let e = <:expr< fun decoder -> match $e$ decoder with
      [ $fields_as_tuple_patt$ -> $recexpr$ ] >> in
  (attrmod, <:patt< Protobuf.Bytes >>, e)
  }


| [%unmatched_vala] -> failwith "pa_deriving_protobuf.of_expression"
]
  in
  let (am, kind, fmt) = fmtrec ~{attrmod=attrmod} ty0 in
  let e = match ty0 with [
    <:ctyp:< ( $list:_$ ) >> | <:ctyp:< { $list:_$ } >>
  | <:ctyp:< [ $list:_$ ] >> | <:ctyp:< [= $list:_$ ] >> ->
    <:expr< let open Pa_ppx_protobuf.Runtime.Decode in $fmt$ >>
  | _ ->
    let loc = loc_of_ctyp ty0 in
    demarshal_to_tuple loc arg [(am, kind, fmt, "v")]
  ] in
  let e =
    let loc = loc_of_ctyp ty0 in
    <:expr< let msg = $str:format_msg arg am.field_name$ in $e$ >> in
  (am, e)
;

value fmt_of_top arg ~{field_name} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  snd (of_expression arg ~{attrmod= { (init_attrmod field_name) with message = True } } params t2)
| t -> snd (of_expression arg ~{attrmod= { (init_attrmod field_name) with message = True } } params t)
]
;

value sig_item_fun0 arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let of_protobuffname = of_protobuf_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< '$PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Protobuf.Decoder.t -> $pty$ >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Protobuf.Decoder.t ->  $(Ctyp.applist ty paramtys)$ >> in
  let e = (of_protobuffname, offtype) in
   (e, None)
;

value gen_sig_items arg td =
  let loc = loc_of_type_decl td in
  let mk1sig (fname, fty) = <:sig_item< value $lid:fname$ : $fty$>> in
  match sig_item_fun0 arg td with [
    (f, None) -> [mk1sig f]
  | (f, Some g) -> [mk1sig f; mk1sig g] ]
;

value sig_items arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  gen_sig_items arg td
}
;

value bare_str_items arg td =
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let tyname = uv tyname in
  if not (Ctxt.is_bare arg tyname) then [] else do {
  assert ([] = uv td.tdPrm) ;
  let tk = td.tdDef in
  let of_protobuffname = of_protobuf_fname arg tyname in
  let bare_name = of_protobuffname^"_bare" in
  let of_e = bare_of_expression arg ~{field_name=tyname} tk in
  [(<:patt< $lid:bare_name$ >>, of_e, <:vala< [] >>)]
}
;

value str_item_funs arg td = do {
  assert (not (match td.tdDef with [ <:ctyp< .. >> | <:ctyp< $_$ == .. >> -> True | _ -> False ])) ;
  let (loc, tyname) = uv td.tdNam in
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let of_protobuffname = of_protobuf_fname arg tyname in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let paramfun_exprs = List.map (PM.arg_expr loc) param_map in
  let body = fmt_of_top arg ~{field_name=tyname} param_map ty in
  let (fun1, ofun2) = sig_item_fun0 arg td in
  let e = 
    let of_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $body$ >> in
    let (_, fty) = fun1 in
    let fty = PM.quantify_over_ctyp param_map fty in
    (<:patt< ( $lid:of_protobuffname$ : $fty$ ) >>,
     Expr.abstract_over (paramtype_patts@paramfun_patts)
       <:expr< fun arg -> $of_e$ arg >>, <:vala< [] >>) in
   [e] @ (bare_str_items arg td)
}
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let sil = gen_sig_items arg td in
    let ((of_protobuffname, offtype), _) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_protobuffname in
    let field_type = PM.quantify_over_ctyp param_map offtype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:of_protobuffname$ = { f: mutable  $field_type$ } ;
      value f : $lid:of_protobuffname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
   as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "protobuf" loc (uv td.tdPrm) in
    let ((of_protobuffname, offtype), oexn) = sig_item_fun0 arg td in
    let modname = Printf.sprintf "M_%s" of_protobuffname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving protobuf] is missing when extending the type " of_protobuffname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map offtype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:of_protobuffname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:of_protobuffname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (of_protobuf_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "protobuf" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let field_name = String.escaped (Pp_MLast.show_longid_lident t) in
    let e = snd (of_expression arg ~{attrmod=init_attrmod field_name} param_map ty) in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    (* remove the catch-branch *)
    let (_, branches) = sep_last branches in 
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps @[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

value str_item_funs arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.str_item_funs arg td
   else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.str_item_funs arg td
   else [])
;

value sig_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.sig_items arg td
   else [])
;

value extend_sig_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.extend_sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.extend_sig_items arg td
   else [])
;

value extend_str_items arg td =
  (if Ctxt.is_plugin_name arg "protobuf" then
     To.extend_str_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "protobuf" then
     Of.extend_str_items arg td
   else [])
;

value str_item_gen_protobuf name arg = fun [
  <:str_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< exception $excon:_$ $itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (str_item_funs arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_protobuf name arg = fun [
  <:sig_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let l = extend_sig_items arg z in
    <:sig_item< declare $list:l$ end >>

| <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    <:sig_item< declare $list:[]$ end >>

| <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>

| _ -> assert False ]
;

value expr_protobuf arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_protobuf" || id = "derive.to_protobuf" ->
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let coercion = monomorphize_ctyp ty in
    let e = To.fmt_to_top arg ~{coercion=coercion} ~{field_name="to_protobuf"} param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (To.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $To.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e

| <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_protobuf" || id = "derive.of_protobuf" ->
  let runtime_module =
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let e = Of.fmt_of_top ~{field_name="of_protobuf"} arg param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (Of.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $Of.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e
| _ -> assert False ]
;

value ctyp_protobuf arg = fun [
  <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "to_protobuf" || id = "derive.to_protobuf" ->
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let argfmttys = List.map (To.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< $ty$ -> Protobuf.Safe.t >>

| <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_protobuf" || id = "derive.of_protobuf" ->
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let argfmttys = List.map (Of.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< Protobuf.Safe.t -> Rresult.result $ty$ string >>

| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "protobuf"
; alternates = []
; options = ["optional" ; "protoc" ; "protoc_import" ; "bare" ]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>) ; ("bare", <:expr< () >> ) ]
; alg_attributes = ["key"; "default"; "encoding"; "bare"; "packed"; "fieldname"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun _ _ -> assert False)
; ctyp = (fun _ _ -> assert False)
; str_item = str_item_gen_protobuf
; sig_item = sig_item_gen_protobuf
})
;


