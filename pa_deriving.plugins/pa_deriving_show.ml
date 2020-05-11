(* camlp5r *)
(* pa_deriving_show.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;

module Ctxt = struct
  include Pa_passthru.Ctxt ;

value with_path ctxt =
  match option ctxt "with_path" with [
    <:expr< True >> -> True
  | <:expr< False >> -> False
  | _ -> failwith "Pa_deriving_show.Ctxt.with_path: option with_path had bad value"
  ]
;

value prefixed_name ctxt id =
  if with_path ctxt then Printf.sprintf "%s.%s" (Ctxt.module_path_s ctxt) id
  else id
;

value module_path ctxt li =
  Ctxt.set_module_path ctxt (Longid.to_string_list li)
;

end ;

value pp_fname arg tyname =
  if tyname = "t" then "pp"
  else "pp_"^tyname
;

value show_fname arg tyname =
  if tyname = "t" then "show"
  else "show_"^tyname
;

value extract_printer (attrs : MLast.attributes_no_anti) =
  let ex1 = fun [
    <:attribute_body< printer $exp:e$ ; >> -> Some e
  | _ -> None
  ] in
  let rec exrec = fun [
    [] -> None
  | [h::t] -> match ex1 (uv h) with [ Some x -> Some x | None -> exrec t ]
  ] in
  exrec attrs
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc ty = <:ctyp< Fmt.t $ty$ >> ; end) ;

value fmt_expression arg ?{coercion} param_map ty0 =
  let runtime_module =
    let loc = loc_of_ctyp ty0 in
    Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = pp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> <:expr< let open $runtime_module$.Fmt in (const string "_") >>
| <:ctyp:< unit >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "()") >>
| <:ctyp:< int >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%d" arg) >>
| <:ctyp:< bool >> -> <:expr<  Fmt.bool >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%ldl" arg) >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%LdL" arg) >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%S" arg) >>
| <:ctyp:< bytes >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%S" (Bytes.to_string arg)) >>
| <:ctyp:< char >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%C" arg) >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%an" nativeint arg) >>
| <:ctyp:< float >> -> <:expr< fun ofmt arg -> let open $runtime_module$.Fmt in (pf ofmt "%F" arg) >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "show" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when id = DC.allowed_attribute (DC.get arg) "show" "opaque" ->
    <:expr< let open $runtime_module$.Fmt in (const string "<opaque>") >>
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "show" "printer" -> e
| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when id = DC.allowed_attribute (DC.get arg) "show" "polyprinter" ->
  let (t0, argtys) = Ctyp.unapplist t in
  let argfmts = List.map fmtrec argtys in
  Expr.applist <:expr< $e$ >> argfmts

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun (ofmt : Format.formatter) arg -> let open $runtime_module$.Fmt in (pf ofmt $str:"@[<2>[%a@,]@]"$ (list ~{sep=semi} $fmt1$) arg) >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun (ofmt : Format.formatter) arg -> let open $runtime_module$.Fmt in (pf ofmt $str:"@[<2>[|%a@,|]@]"$ (array ~{sep=semi} $fmt1$) arg) >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< fun (ofmt : Format.formatter) arg -> let open $runtime_module$.Fmt in (pf ofmt $str:"ref (%a)"$ $fmt1$ arg.val) >>

| <:ctyp:< lazy_t $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun (ofmt : Format.formatter) arg ->
    if Lazy.is_val arg then
      $fmt1$ ofmt (Lazy.force arg)
    else let open $runtime_module$.Fmt in (const string "<not evaluated>") ofmt () >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun ofmt -> fun [
          None -> let open $runtime_module$.Fmt in (const string "None") ofmt ()
        | Some arg -> let open $runtime_module$.Fmt in (pf ofmt "(Some %a)" $fmt1$ arg)
      ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun ofmt -> fun [
          Result.Ok ok -> let open $runtime_module$.Fmt in (pf ofmt "(Ok %a)" $(fmtrec ty1)$ ok)
        | Result.Error e -> let open $runtime_module$.Fmt in (pf ofmt "(Error %a)" $(fmtrec ty2)$ e)
      ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.show: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $lid:lid$ >> ->
  let fname = pp_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = pp_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< $_$ -> $_$ >> -> <:expr< let open $runtime_module$.Fmt in (const string "<fun>") >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let fmtstring = Printf.sprintf "(@[%s@])"
        (String.concat ",@ " (List.map (fun _ -> "%a") vars)) in
    let e = List.fold_left2 (fun e f v -> <:expr< $e$ $f$ $lid:v$ >>)
        <:expr< pf ofmt $str:fmtstring$ >> fmts vars in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    <:expr< fun (ofmt : Format.formatter) ($list:varpats$) -> let open $runtime_module$.Fmt in ($e$) >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    (loc, cid, <:vala< [TyRec _ fields] >>, None, _) ->
    let cid = uv cid in
    let prefix_txt = (Ctxt.prefixed_name arg cid)^" " in
    let (recpat, body) = fmt_record ~{without_path=True} ~{prefix_txt=prefix_txt} ~{bracket_space=""} loc arg (uv fields) in

    let conspat = <:patt< $uid:cid$ $recpat$ >> in
    (conspat, <:vala< None >>, body)

  | (loc, cid, tyl, None, attrs) ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> varpats in
    match extract_printer (uv attrs) with [
      Some printerf -> 
      let varexprs = List.map (fun v -> <:expr< $lid:v$ >>) vars in
      let tupleexpr = match varexprs with [
        [] ->  <:expr< () >>
      | [e] -> e
      | l ->  <:expr< ( $list:varexprs$ ) >>
      ] in
      (conspat, <:vala< None >>, <:expr< $printerf$ ofmt $tupleexpr$ >>)
    | None ->
    let fmts = List.map fmtrec tyl in
    let fmtstring =
      if vars = [] then
        Printf.sprintf "@[<2>%s@]" (Ctxt.prefixed_name arg cid)
      else if List.length vars = 1 then
        Printf.sprintf "(@[<2>%s@ %s)@]" (Ctxt.prefixed_name arg cid)
        (String.concat ",@ " (List.map (fun _ -> "%a") vars))
      else
        Printf.sprintf "(@[<2>%s@ (@,%s@,))@]" (Ctxt.prefixed_name arg cid)
        (String.concat ",@ " (List.map (fun _ -> "%a") vars))
    in
    let e = List.fold_left2 (fun e f v -> <:expr< $e$ $f$ $lid:v$ >>)
        <:expr< pf ofmt $str:fmtstring$ >> fmts vars in
    (conspat, <:vala< None >>, <:expr< let open $runtime_module$.Fmt in ($e$) >>)
    ]
  | (_, _, _, Some _, _) -> assert False
  ]) l in
  <:expr< fun ofmt -> fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let fmtstring =
      if vars = [] then
        Printf.sprintf "@[<2>`%s@]" cid
      else if List.length vars = 1 then
        Printf.sprintf "@[<2>`%s (@,%s@,)@]" cid
          (String.concat ",@ " (List.map (fun _ -> "%a") vars))
      else
        Printf.sprintf "@[<2>`%s (@,%s@,)@]" cid
          (String.concat ",@ " (List.map (fun _ -> "%a") vars))
    in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> varpats in
    let e = List.fold_left2 (fun e f v -> <:expr< $e$ $f$ $lid:v$ >>)
        <:expr< pf ofmt $str:fmtstring$ >> fmts vars in
    (conspat, <:vala< None >>, <:expr< let open $runtime_module$.Fmt in ($e$) >>)

  | PvInh _ ty ->
    let lili = match fst (Ctyp.unapplist ty) with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some li, lid)
    | [%unmatched_vala] -> failwith "fmt_expression-PvInh"
     ] in
    let conspat = <:patt< ( # $lilongid:lili$ as z ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< let open $runtime_module$.Fmt in ($fmtf$ ofmt z) >>)
  ]) l in
  <:expr< fun ofmt -> fun [ $list:branches$ ] >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record ~{without_path=False} ~{prefix_txt=""} ~{bracket_space=" "} loc arg fields in
  let recpat = match coercion with [ None -> recpat | Some ty -> <:patt< ( $recpat$ : $ty$ ) >> ] in
  <:expr< fun ofmt $recpat$ -> $body$ >>
| [%unmatched_vala] -> failwith "pa_deriving_show.fmt_expression"
| ty -> failwith (Printf.sprintf "pa_deriving_show.fmt_expression: failed on type %s" (Ctyp.print ty))
]
and fmt_record ~{without_path} ~{prefix_txt} ~{bracket_space} loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "v_%s" fname, fmtrec ty)) fields in

  let field_text i f =
    if not without_path && i = 0 then Ctxt.prefixed_name arg f
    else f in
  let fmt = Printf.sprintf "@[<2>%s{%s%s%s}@]"
      prefix_txt
      bracket_space
      (String.concat ";@ " (List.mapi (fun i (f, _, _) ->
                            Printf.sprintf "@[%s =@ %s@]" (field_text i f) "%a") labels_vars_fmts))
      bracket_space
  in
  let e = List.fold_left (fun e (f,v,fmtf) ->
      <:expr< $e$ $fmtf$ $lid:v$ >>)
      <:expr< pf ofmt $str:fmt$ >> labels_vars_fmts in
  let pl = List.map (fun (f, v, _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  (<:patt< { $list:pl$ } >>, <:expr< let open $runtime_module$.Fmt in ($e$) >>)
in fmtrec ?{coercion=coercion} ty0
;

value fmt_top arg ~{coercion} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  let arg = match t1 with [
    <:ctyp< $longid:li$ . $lid:_$ >> -> Ctxt.module_path arg li
  | _ -> arg
  ] in
  fmt_expression arg ~{coercion=coercion} params t2
| t -> fmt_expression arg ~{coercion=coercion} params t
]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "show" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let ppfname = pp_fname arg tyname in
  let showfname = show_fname arg tyname in
  let paramtys = List.map (PM.param_ctyp loc) param_map in
  let argfmttys = List.map (PM.arg_ctyp loc) param_map in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let ppftype = Ctyp.arrows_list loc argfmttys <:ctyp< Fmt.t $(Ctyp.applist ty paramtys)$ >> in
  let showftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Stdlib.String.t >> in
  [(ppfname, ppftype) ;
   (showfname, showftype)]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "show" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let coercion =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    monomorphize_ctyp (Ctyp.applist ty paramtys) in
  let ppfname = pp_fname arg tyname in
  let showfname = show_fname arg tyname in
  let e = fmt_top arg ~{coercion=coercion} param_map ty in

  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
  let paramfun_exprs = List.map (PM.arg_expr loc) param_map in
  let ppfexp = <:expr< $lid:ppfname$ >> in

  [(ppfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun (ofmt : Format.formatter) arg -> $e$ ofmt arg >>);
   (showfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg -> Format.asprintf "%a" $(Expr.applist ppfexp paramfun_exprs)$ arg >>)]
;

value str_item_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "show" loc (uv td.tdPrm) in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  PM.wrap_type_constraints loc param_map funs types
;

value sig_items arg td =
  let loc = loc_of_type_decl td in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;


value extend_sig_items arg si = match si with [
  <:sig_item< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >>
  as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "show" loc (uv td.tdPrm) in
    let sil = sig_items arg td in
    let (ppfname, ppftype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" ppfname in
    let field_type = PM.quantify_over_ctyp param_map ppftype in
    [ <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:ppfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:ppfname$ ;
    end >> :: sil ]
| _ -> assert False
]
;

value extend_str_items arg si = match si with [
  <:str_item:< type $tp:_$ $list:_$ = $priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >>
  as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "show" loc (uv td.tdPrm) in
    let (ppfname, ppftype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" ppfname in
    let showfname = show_fname arg (uv tyname) in
    let showfexp =
      let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
      let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
      let paramfun_exprs = List.map (PM.arg_expr loc) param_map in
      let ppfexp = <:expr< $uid:modname$ . f . $uid:modname$ . f >> in
      Expr.abstract_over (paramtype_patts@paramfun_patts)
        <:expr< fun arg -> Format.asprintf "%a" $(Expr.applist ppfexp paramfun_exprs)$ arg >> in

    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving show] is missing when extending the type " ppfname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map ppftype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:ppfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< value $lid:ppfname$ x = $uid:modname$ . f . $uid:modname$ . f x >> ;
      <:str_item< value $lid:showfname$ = $showfexp$ >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (pp_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some li -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "show" loc params in
    let ec2gc = fun [
      EcTuple gc -> [gc]
    | EcRebind _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = fmt_expression arg param_map ty in
    let branches = match e with [
      <:expr< fun ofmt -> fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps @[<:expr< ofmt >> ;  <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun ofmt -> fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

| _ -> assert False
]
;

value str_item_gen_show name arg = fun [
  <:str_item:< type $_tp:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $tp:_$ $list:_$ = $_$ == $priv:_$ .. $_itemattrs:_$ >>
  as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let l = List.concat (List.map (str_item_funs arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_show name arg = fun [
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

value expr_show arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "show" || id = "derive.show" ->
    let loc = loc_of_ctyp ty in
    let param_map = ty |> type_params |> PM.make_of_ids in
    let coercion = monomorphize_ctyp ty in
    let e = fmt_top arg ~{coercion=coercion} param_map ty in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $PM.type_id p$) >>) param_map in
    let e = <:expr< fun arg -> Format.asprintf "%a" $e$ arg >> in
    Expr.abstract_over (paramtype_patts@parampats) e

| _ -> assert False ]
;

value ctyp_show arg = fun [
  <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "show" || id = "derive.show" ->
    let param_map = ty |> type_params |> PM.make_of_ids in
    let argfmttys = List.map (PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< $ty$ -> Stdlib.String.t >>

| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "show"
; alternates = []
; options = ["with_path"; "optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ; ("with_path", <:expr< True >>) ]
; alg_attributes = ["opaque"; "printer"; "polyprinter"; "nobuiltin"]
; expr_extensions = ["show"]
; ctyp_extensions = ["show"]
; expr = expr_show
; ctyp = ctyp_show
; str_item = str_item_gen_show
; sig_item = sig_item_gen_show
})
;

