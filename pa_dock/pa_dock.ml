(* camlp5r *)
(* pa_dock.ml,v *)
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

value lex_locations gram is = do {
  let glexer = Grammar.glexer gram in
  let (tokstrm, loct) = glexer.Plexing.tok_func is in
  while (match Stream.peek tokstrm with [ Some("EOI",_) -> False | t ->  True ]) do { Stream.junk tokstrm } ;
  loct
}
;

value comments_of_string s =
  let loct = s |> Stream.of_string |> lex_locations Pcaml.gram in
  let ll = List.map (fun [
      None -> []
    | Some loc when Ploc.comment loc = "" -> []
    | Some loc ->
      let s =  Ploc.comment loc in
      let loc = Comment_lexer.adjust_loc s loc in
      Comment_lexer.tokenize_comment (s, loc)
    ]) (Array.to_list loct.Plexing.Locations.locations.val) in
  List.concat ll
;

value comments_of_file f = do {
  let fname = f |> Fpath.v in
  fname |> Bos.OS.Path.must_exist |> Rresult.R.failwith_error_msg ;
  let s = fname |> Bos.OS.File.read
             |> Rresult.R.get_ok in
  comments_of_string s
}
;

module CM = Map.Make(struct type t = int ; value compare = Stdlib.compare ; end) ;

value make_comment_map l =
  List.fold_left (fun m ((s : string), i) -> CM.add i s m) CM.empty l
;

value comments_between (m : CM.t string) i j = do {
  assert (i <= j) ;
  let (_, iopt, rest) = CM.split i m in
  let (want, jopt, _) = CM.split j rest in
  (match iopt with [ None -> [] | Some x -> [(i, x)] ])@
  (CM.bindings want)
}
;

type t = {
  cm : mutable CM.t string
} ;
type scratchdata_t += [ Pa_dock of t ] ;

value get arg =
  match Ctxt.scratchdata arg "dock" with [
    Pa_dock dc -> dc.cm
  | _ -> assert False
  ]
;

value update arg newv =
  match Ctxt.scratchdata arg "dock" with [
    Pa_dock dc -> dc.cm := newv
  | _ -> assert False
  ]
;

value init arg m =
   Ctxt.init_scratchdata arg "dock" (Pa_dock { cm = m })
;

value is_comment s =
  String.length s >= 2 && "(*" = String.sub s 0 2 ;

value is_doc_comment s =
  String.length s >= 4 && "(**" = String.sub s 0 3  && "(***" <> String.sub s 0 4 ;

value not_is_doc_comment s = not (is_doc_comment s) ;

value is_blank_line s =
  not (is_comment s) &&
  Comment_lexer.string_count s '\n' > 1 ;

value f_not p x = not (p x) ;
value f_or p1 p2 x = (p1 x) || (p2 x);

value split_at_first_doc_comment l =
  let rec srec acc = fun [
    ([ h :: t ] as l) when is_doc_comment h -> (List.rev acc, l)
  | ([ h :: t ] as l) -> srec [ h :: acc ] t
  | [] -> (List.rev acc, [])
  ]
 in srec [] l
;

value sig_item_apportion_interior_comments l =
  let (before_doc, maybe_doc) = split_at_first_doc_comment l in
  let (before, rest) = match maybe_doc with [
    [] -> ([], [])
  | ([ h :: t ] as l) -> do {
      assert (is_doc_comment h) ;
      if List.for_all (f_not (f_or is_blank_line is_comment)) before_doc then
        ([h], t)
      else ([], l)
    }
  ] in
  let (rev_after_doc, rev_maybe_doc) = split_at_first_doc_comment (List.rev rest) in
  let (after, rev_rest) = match rev_maybe_doc with [
    [] -> ([], [])
  | [ h :: t ] as l -> do {
      assert(is_doc_comment h) ;
      if List.for_all (f_not is_blank_line) rev_after_doc then
        ([h], t)
      else ([], l)
    }
  ] in
  let rest = List.rev (Std.filter is_doc_comment rev_rest) in
  (before, rest, after)
;

value str_item_apportion_interior_comments l =
  let (rev_after_doc, rev_maybe_doc) = split_at_first_doc_comment (List.rev l) in
  let (after, rev_rest) = match rev_maybe_doc with [
    [] -> (None, [])
  | [ h :: t ] as l -> do {
      assert(is_doc_comment h) ;
      if List.for_all (f_not is_blank_line) rev_after_doc then
        (Some h, t)
      else (None, l)
    }
  ] in
  let rest = List.rev (Std.filter is_doc_comment rev_rest) in
  (rest, after)
;

value flatten_structure l =
  let rec frec acc = fun [
    [] -> List.rev acc
  | [ <:str_item< declare $list:l$ end >> :: t ] ->
      frec acc (l@t)
  | [ h :: t ] -> frec [h :: acc] t
  ]
  in frec [] l
;

value strip_comment_marks s = do {
  let slen = String.length s in
  assert (slen > String.length "(***)") ;
  assert (String.sub s 0 3 = "(**") ;
  assert (String.sub s (slen -2) 2 = "*)") ;
  String.escaped (String.sub s 3 (slen - 5))
}
;

value str_item_floating_attribute s =
  let loc = Ploc.dummy in
  let s = strip_comment_marks s in
  (<:str_item< [@@@ "ocaml.text" $str:s$ ; ] >>, loc)
;

value class_str_item_floating_attribute s =
  let loc = Ploc.dummy in
  let s = strip_comment_marks s in
  <:class_str_item< [@@@ "ocaml.text" $str:s$ ; ] >>
;

value attr_doc_comment loc s =
  let s = strip_comment_marks s in
  let a = <:attribute_body< "ocaml.doc" $str:s$ ; >> in
  <:vala< a >>
;

value class_str_item_wrap_itemattr a si = match si with [
  <:class_str_item:< [@@@ $_attribute:_$ ] >> -> assert False
| <:class_str_item< declare $_list:st$ end >> -> assert False

| <:class_str_item:< inherit $_!:ovf$ $ce$ $_opt:pb$ $itemattrs:attrs$ >> ->
  <:class_str_item< inherit $_!:ovf$ $ce$ $_opt:pb$ $itemattrs:attrs@[ a ]$ >>

| <:class_str_item:< value $_!:ovf$ $_flag:mf$ $_lid:lab$ = $e$ $itemattrs:attrs$ >> ->
  <:class_str_item< value $_!:ovf$ $_flag:mf$ $_lid:lab$ = $e$ $itemattrs:attrs@[ a ]$ >>

| <:class_str_item:< value virtual $_flag:mf$ $_lid:lab$ : $t$ $itemattrs:attrs$ >> ->
  <:class_str_item< value virtual $_flag:mf$ $_lid:lab$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:class_str_item:< method virtual $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs$ >> ->
  <:class_str_item< method virtual $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:class_str_item:<
    method $_!:ovf$ $_priv:pf$ $_lid:l$ $_opt:topt$ = $e$ $itemattrs:attrs$ >> ->
  <:class_str_item<
    method $_!:ovf$ $_priv:pf$ $_lid:l$ $_opt:topt$ = $e$ $itemattrs:attrs@[ a ]$ >>

| <:class_str_item:< type $t1$ = $t2$ $itemattrs:attrs$ >> ->
  <:class_str_item< type $t1$ = $t2$ $itemattrs:attrs@[ a ]$ >>
| <:class_str_item:< [%% $_extension:e$ ] >> ->
  Ploc.raise loc (Invalid_argument "class_str_item-%%extension")
]
;

value str_item_wrap_itemattr a si = match si with [
  <:str_item:< declare $list:_$ end >> -> assert False
| <:str_item< [@@@ $_attribute:attr$ ] >> -> assert False

| <:str_item:< exception $excon:ec$ $itemattrs:item_attrs$ >> ->
  <:str_item< exception $excon:ec$ $itemattrs:item_attrs@[ a ]$ >>

| <:str_item:< external $lid:i$ : $t$ = $list:pd$ $itemattrs:attrs$ >> ->
  <:str_item< external $lid:i$ : $t$ = $list:pd$ $itemattrs:attrs@[ a ]$ >>

| <:str_item:< include $me$ $itemattrs:attrs$ >> ->
  <:str_item< include $me$ $itemattrs:attrs @[ a ]$ >>

| <:str_item:< module $flag:r$ $list:l$ >> ->
  let (last, l) = sep_last l in
  let (id, me, attrs) = last in
  let attrs = (uv attrs)@[ a ] in
  let last = (id, me, <:vala< attrs >>) in
  let l = l @ [ last ] in
  <:str_item< module $flag:r$ $list:l$ >>

| <:str_item:< module type $_:i$ = $mt$ $itemattrs:attrs$ >> ->
  <:str_item< module type $_:i$ = $mt$ $itemattrs:attrs@[ a ]$ >>

| <:str_item:< module type $_:i$ $itemattrs:attrs$ >> ->
  <:str_item< module type $_:i$ $itemattrs:attrs@[ a ]$ >>
| <:str_item:< open $_!:ovf$ $me$ $itemattrs:attrs$ >> ->
  <:str_item< open $_!:ovf$ $me$ $itemattrs:attrs@[ a ]$ >>

| <:str_item:< type $_flag:nrfl$ $list:tdl$ >> ->
  let (last, tdl) = sep_last tdl in
  let attrs = uv last.tdAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with tdAttributes = <:vala< attrs >> } in
  let tdl = tdl @ [ last ] in
  <:str_item< type $_flag:nrfl$ $list:tdl$ >>

| <:str_item:< type $_lilongid:tp$ $_list:pl$ += $_priv:pf$ [ $_list:ecs$ ] $itemattrs:attrs$ >> ->
  <:str_item< type $_lilongid:tp$ $_list:pl$ += $_priv:pf$ [ $_list:ecs$ ] $itemattrs:attrs@[ a ]$ >>

| <:str_item:< value $_flag:r$ $list:l$ >> ->
  let (last, l) = sep_last l in
  let (p,e,attrs) = last in
  let attrs = (uv attrs)@[ a ] in
  let last = (p,e, <:vala< attrs >> ) in
  let l = l @ [ last ] in
  <:str_item< value $_flag:r$ $list:l$ >>

| <:str_item:< # $_lid:n$ $_opt:dp$ >> as z -> z
| <:str_item< # $_str:s$ $_list:sil$ >> as z -> z

| <:str_item:< $exp:e$ $itemattrs:attrs$ >> ->
  <:str_item< $exp:e$ $itemattrs:attrs@[ a ]$ >>

| <:str_item:< [%% $_extension:e$ ] $itemattrs:attrs$ >> ->
  <:str_item< [%% $_extension:e$ ] $itemattrs:attrs@[ a ]$ >>

| <:str_item:< class $list:cd$ >> ->
  let (last, cd) = sep_last cd in
  let attrs = uv last.ciAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with ciAttributes = <:vala< attrs >> } in
  let cd = cd @ [ last ] in
  <:str_item< class $list:cd$ >>

| <:str_item:< class type $list:ctd$ >> ->
  let (last, ctd) = sep_last ctd in
  let attrs = uv last.ciAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with ciAttributes = <:vala< attrs >> } in
  let ctd = ctd @ [ last ] in
  <:str_item< class type $list:ctd$ >>


| [%unmatched_vala] -> Ploc.raise (loc_of_str_item si) (Failure "pa_dock.str_item_wrap_itemattr")

]
;

value rewrite_gc arg ((loc, ci, tyl, rto, attrs) : generic_constructor) maxpos = 
  let startpos = Ploc.last_pos loc in
  let l = List.map snd (comments_between (get arg) startpos maxpos) in
  let l = Std.filter is_doc_comment l in
  let newattrs = List.map (attr_doc_comment loc) l in
  let attrs = <:vala< (uv attrs) @ newattrs >> in
  ((loc, ci, tyl, rto, attrs), Ploc.first_pos loc)
;

value rewrite_field arg ((loc, f, m, ty, attrs) : (loc * string * bool * ctyp * attributes)) maxpos = 
  let startpos = Ploc.last_pos loc in
  let l = List.map snd (comments_between (get arg) startpos maxpos) in
  let l = Std.filter is_doc_comment l in
  let newattrs = List.map (attr_doc_comment loc) l in
  let attrs = <:vala< (uv attrs) @ newattrs >> in
  ((loc, f, m, ty, attrs), Ploc.first_pos loc)
;

value rewrite_type_decl arg td maxpos = match td.tdDef with [
  <:ctyp:< [ $list:l$ ] >> ->
  let (l, _) = List.fold_right (fun gc (acc, maxpos) ->
        let (gc, maxpos) = rewrite_gc arg gc maxpos in
        ([ gc :: acc], maxpos)
    ) l ([], maxpos) in
  { (td) with tdDef = <:ctyp< [ $list:l$ ] >> }

| <:ctyp:< { $list:l$ } >> ->
  let (l, _) = List.fold_right (fun gc (acc, maxpos) ->
        let (gc, maxpos) = rewrite_field arg gc maxpos in
        ([ gc :: acc], maxpos)
    ) l ([], Ploc.last_pos loc) in
  { (td) with tdDef = <:ctyp< { $list:l$ } >> }

| _ -> td
]
;

value rewrite_type_decls arg maxpos tdl =
  let (tdl, _) = List.fold_right (fun td (acc, maxpos) ->
    let td = rewrite_type_decl arg td maxpos in
    let maxpos = Ploc.first_pos (loc_of_type_decl td) in
    ([ td :: acc], maxpos)) tdl ([], maxpos)
  in tdl
;

value rewrite_str_item_pair arg ((h1 : str_item), loc1) (h2, loc2) =
  let ep1 = Ploc.last_pos loc1 in
  let bp2 = Ploc.first_pos loc2 in
  let l = List.map snd (comments_between (get arg) ep1 bp2) in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:str_item< [@@@ $_attribute:_$ ] >>) -> ([str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], str_item_wrap_itemattr (attr_doc_comment loc2 s) h2)
  ] in
  ((h1, loc1), floating@more_floating, (h2, loc2))
;

value rewrite_class_str_item_pair arg (h1 : class_str_item) h2 =
  let loc1 = loc_of_class_str_item h1 in
  let loc2 = loc_of_class_str_item h2 in
  let ep1 = Ploc.last_pos loc1 in
  let bp2 = Ploc.first_pos loc2 in
  let l = List.map snd (comments_between (get arg) ep1 bp2) in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map class_str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:class_str_item< [@@@ $_attribute:_$ ] >>) -> ([class_str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], class_str_item_wrap_itemattr (attr_doc_comment loc2 s) h2)
  ] in
  (h1, floating@more_floating, h2)
;

value rewrite_structure arg loc l =
  let rec rerec = fun [
    [ h1 ; h2 :: t ] ->
    let (h1, floating, h2) = rewrite_str_item_pair arg (h1, loc_of_str_item h1) (h2, loc_of_str_item h2) in
    [ (fst h1) ] @ (List.map fst floating) @ (rerec [ (fst h2) :: t ])
  | [ si ] -> [ si ]
  | [] -> []
  ] in
  rerec l
;

value rewrite_class_structure arg loc l =
  let rec rerec = fun [
    [ h1 ; h2 :: t ] ->
    let (h1, floating, h2) = rewrite_class_str_item_pair arg h1 h2 in
    [ h1 ] @ floating @ (rerec [ h2 :: t ])
  | [ si ] -> [ si ]
  | [] -> []
  ] in
  rerec l
;

value rewrite_first_implem_item arg ep1 (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = List.map snd (comments_between (get arg) ep1 bp2) in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:str_item< [@@@ $_attribute:_$ ] >>) -> ([str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], str_item_wrap_itemattr (attr_doc_comment loc2 s) h2)
  ] in
  (floating@more_floating, (h2, loc2))
;

value rec rewrite_implem arg (sil, status) = 
  let rec rerec = fun [
    [ h1 ; h2 :: t ] ->
    let (h1, floating, h2) = rewrite_str_item_pair arg h1 h2 in
    [ h1 ] @ floating @ (rerec [ h2 :: t ])
  | [ si ] -> [ si ]
  | [] -> []
  ] in
  match sil with [
    [] -> assert False
  | [ h :: t ] ->
    let (floating, h) = rewrite_first_implem_item arg 0 h in
    (floating @ rerec [ h :: t], status)
  ]
;

value flatten_signature l =
  let rec frec acc = fun [
    [] -> List.rev acc
  | [ <:sig_item< declare $list:l$ end >> :: t ] ->
      frec acc (l@t)
  | [ h :: t ] -> frec [h :: acc] t
  ]
  in frec [] l
;

value rec rewrite_signature arg = fun [
  z ->
  z
]
;

value wrap_implem arg z = do {
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let fname = Ctxt.filename arg in
  let l = comments_of_file fname in
  let m = make_comment_map l in
  init arg m ;
  (sil, status) |> rewrite_implem arg |> Pa_passthru.implem0 arg 
}
;

value wrap_interf arg z = do {
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let fname = Ctxt.filename arg in
  let l = comments_of_file fname in
  let m = make_comment_map l in
  init arg m ;
  Pa_passthru.interf0 arg (sil, status)
}
;

value install () = 
let ef = EF.mk () in 

let ef = EF.{ (ef) with
            class_expr = extfun ef.class_expr with [
    <:class_expr:< object $_opt:cspo$ $list:cf$ end >> ->
    fun arg ->
    let cf = rewrite_class_structure arg loc cf in
    Some <:class_expr< object $_opt:cspo$ $list:cf$ end >>
  ] } in

let ef = EF.{ (ef) with
            module_expr = extfun ef.module_expr with [
    <:module_expr:< struct $list:l$ end >> ->
    fun arg ->
    let l = rewrite_structure arg loc (flatten_structure l) in
    Some <:module_expr:< struct $list:l$ end >>
  ] } in

let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< type $_flag:nrfl$ $list:tdl$ >> ->
    fun arg ->
    let tdl = rewrite_type_decls arg (Ploc.last_pos loc) tdl in
    Some <:str_item< type $_flag:nrfl$ $list:tdl$ >>
  ] } in

let ef = EF.{ (ef) with
            signature = extfun ef.signature with [
    z ->
    fun arg ->
      Some (rewrite_signature arg (flatten_signature z))
  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg -> 
      Some (wrap_implem arg z)
  ] } in

let ef = EF.{ (ef) with
              interf = extfun ef.interf with [
    z ->
    fun arg -> 
      Some (wrap_interf arg z)
  ] } in

  Pa_passthru.(install { name = "pa_dock_doc_comment" ; ef = ef ; pass = Some 0 ; before = [] ; after = [] })
;

install();
