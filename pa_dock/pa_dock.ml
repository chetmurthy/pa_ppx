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

value stashed_file_contents = ref "" ;

value comments_of_file f =
  let s =
    if f = "file://stashed" then stashed_file_contents.val
    else
      let fname = f |> Fpath.v in do {
      fname |> Bos.OS.Path.must_exist |> Rresult.R.failwith_error_msg ;
      fname |> Bos.OS.File.read
      |> Rresult.R.get_ok } in
  comments_of_string s
;

module CM = Map.Make(struct type t = int ; value compare = Stdlib.compare ; end) ;

value make_comment_map l =
  List.fold_left (fun m ((s : string), i) -> CM.add i s m) CM.empty l
;

value comments_between0 (m : CM.t string) i j = do {
  assert (i <= j) ;
  let (_, iopt, rest) = CM.split i m in
  let (want, jopt, _) = CM.split j rest in
  (match iopt with [ None -> [] | Some x -> [(i, x)] ])@
  (CM.bindings want)
}
;

value comments_between m i j =
  (comments_between0 m i j)
|> List.map (fun (ofs, s) ->
      let slen = String.length s in
      (Ploc.make_unlined (ofs, ofs+slen), s))
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

value is_comment (_, s) =
  String.length s >= 2 && "(*" = String.sub s 0 2 ;

value is_doc_comment (_, s) =
  String.length s >= 4 && "(**" = String.sub s 0 3  && "(***" <> String.sub s 0 4 ;

value not_is_doc_comment s = not (is_doc_comment s) ;

value is_blank_line s =
  not (is_comment s) &&
  Comment_lexer.string_count (snd s) '\n' > 1 ;

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

value sig_item_apportion_leading_comments l =
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

value sig_item_apportion_interior_comments l =
  let (before_doc, maybe_doc) = split_at_first_doc_comment l in
  let (before, rest) = match maybe_doc with [
    [] -> (None, [])
  | ([ h :: t ] as l) -> do {
      assert (is_doc_comment h) ;
      if List.for_all (f_not (f_or is_blank_line is_comment)) before_doc then
        (Some h, t)
      else (None, l)
    }
  ] in
  let (rev_after_doc, rev_maybe_doc) = split_at_first_doc_comment (List.rev rest) in
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

value strip_comment_marks s = do {
  let slen = String.length s in
  assert (slen > String.length "(***)") ;
  assert (String.sub s 0 3 = "(**") ;
  assert (String.sub s (slen -2) 2 = "*)") ;
  String.escaped (String.sub s 3 (slen - 5))
}
;

value str_item_floating_attribute (loc, s) =
  let s = strip_comment_marks s in
  (<:str_item< [@@@ "ocaml.text" $str:s$ ; ] >>, loc)
;

value sig_item_floating_attribute (loc, s) =
  let s = strip_comment_marks s in
  (<:sig_item< [@@@ "ocaml.text" $str:s$ ; ] >>, loc)
;

value class_str_item_floating_attribute (loc, s) =
  let s = strip_comment_marks s in
  <:class_str_item< [@@@ "ocaml.text" $str:s$ ; ] >>
;

value class_sig_item_floating_attribute (loc, s) =
  let s = strip_comment_marks s in
  <:class_sig_item< [@@@ "ocaml.text" $str:s$ ; ] >>
;

value attr_doc_comment (loc, s) =
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

| <:str_item:< exception $_uid:ci$ of $list:cal$ $algattrs:alg_attrs$ $itemattrs:item_attrs$ >> ->
  <:str_item:< exception $_uid:ci$ of $list:cal$ $algattrs:alg_attrs@[ a ]$ $itemattrs:item_attrs$ >>

| <:str_item:< exception $_uid:ci$ $algattrs:alg_attrs$ $itemattrs:item_attrs$ >> ->
  <:str_item:< exception $_uid:ci$ $algattrs:alg_attrs@[ a ]$ $itemattrs:item_attrs$ >>

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

value sig_item_wrap_itemattr a si = match si with [
  <:sig_item< declare $_list:st$ end >> -> assert False
| <:sig_item< [@@@ $_attribute:attr$ ] >> -> assert False

| <:sig_item:< exception $_uid:ci$ of $list:cal$ $algattrs:alg_attrs$ $itemattrs:item_attrs$ >> ->
  <:sig_item:< exception $_uid:ci$ of $list:cal$ $algattrs:alg_attrs@[ a ]$ $itemattrs:item_attrs$ >>

| <:sig_item:< exception $_uid:ci$ $algattrs:alg_attrs$ $itemattrs:item_attrs$ >> ->
  <:sig_item:< exception $_uid:ci$ $algattrs:alg_attrs@[ a ]$ $itemattrs:item_attrs$ >>

| <:sig_item:< external $_lid:i$ : $t$ = $_list:pd$ $itemattrs:attrs$ >> ->
  <:sig_item< external $_lid:i$ : $t$ = $_list:pd$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< include $mt$ $itemattrs:attrs$ >> ->
  <:sig_item< include $mtyp:mt$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< module $_flag:rf$ $list:l$ >> ->
  let (last, l) = sep_last l in
  let (na, ty, attrs) = last in
  let attrs = <:vala< (uv attrs) @ [ a ] >> in
  let last = (na, ty, attrs) in
  let l = l @ [ last ] in
  <:sig_item< module $_flag:rf$ $list:l$ >>

| <:sig_item:< module $_uid:i$ := $longid:li$ $itemattrs:attrs$ >> ->
  <:sig_item< module $_uid:i$ := $longid:li$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< module type $_:i$ = $mt$ $itemattrs:attrs$ >> ->
  <:sig_item< module type $_:i$ = $mt$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< module type $_:i$ $itemattrs:attrs$ >> ->
  <:sig_item< module type $_:i$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< module alias $_:i$ = $_longid:li$ $itemattrs:attrs$ >> ->
  <:sig_item< module alias $_:i$ = $_longid:li$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< open $longid:i$ $itemattrs:attrs$ >> ->
  <:sig_item< open $longid:i$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< type $_flag:nrfl$ $list:tdl$ >> ->
  let (last, tdl) = sep_last tdl in
  let attrs = uv last.tdAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with tdAttributes = <:vala< attrs >> } in
  let tdl = tdl @ [ last ] in
  <:sig_item< type $_flag:nrfl$ $list:tdl$ >>

| <:sig_item:< type $_lilongid:tp$ $_list:pl$ += $_priv:pf$ [ $_list:ecs$ ] $itemattrs:attrs$ >> ->
  <:sig_item< type $_lilongid:tp$ $_list:pl$ += $_priv:pf$ [ $_list:ecs$ ] $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< value $_lid:i$ : $t$ $itemattrs:attrs$ >> ->
  <:sig_item< value $_lid:i$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:sig_item< # $_lid:_$ $_opt:_$ >> as z -> z

| <:sig_item< # $_str:_$ $_list:_$ >> as z -> z

| <:sig_item:< [%% $_extension:e$ ] $itemattrs:attrs$ >> ->
  <:sig_item< [%% $_extension:e$ ] $itemattrs:attrs@[ a ]$ >>

| <:sig_item:< class $list:cd$ >> ->
  let (last, cd) = sep_last cd in
  let attrs = uv last.ciAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with ciAttributes = <:vala< attrs >> } in
  let cd = cd @ [ last ] in
  <:sig_item< class $list:cd$ >>

| <:sig_item:< class type $list:ctd$ >> ->
  let (last, ctd) = sep_last ctd in
  let attrs = uv last.ciAttributes in
  let attrs = attrs @ [ a ] in
  let last = { (last) with ciAttributes = <:vala< attrs >> } in
  let ctd = ctd @ [ last ] in
  <:sig_item< class type $list:ctd$ >>

| [%unmatched_vala] -> Ploc.raise (loc_of_sig_item si) (Failure "pa_dock.sig_item_wrap_itemattr")

]
;

value class_sig_item_wrap_itemattr a si = match si with [
  <:class_sig_item< declare $_list:_$ end >> -> assert False
| <:class_sig_item< [@@@ $_attribute:_$ ] >> -> assert False

| <:class_sig_item:< inherit $cs$ $itemattrs:attrs$ >> ->
  <:class_sig_item< inherit $cs$ $itemattrs:attrs@[ a ]$ >>

| <:class_sig_item:< value $_flag:mf$ $_flag:vf$  $_lid:l$ : $t$ $itemattrs:attrs$ >> ->
  <:class_sig_item< value $_flag:mf$ $_flag:vf$  $_lid:l$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:class_sig_item:< method virtual $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs$ >> ->
  <:class_sig_item< method virtual $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:class_sig_item:< method $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs$ >> ->
  <:class_sig_item< method $_flag:pf$ $_lid:l$ : $t$ $itemattrs:attrs@[ a ]$ >>

| <:class_sig_item:< type $t1$ = $t2$ $itemattrs:attrs$ >> ->
  <:class_sig_item< type $t1$ = $t2$ $itemattrs:attrs@[ a ]$ >>

| <:class_sig_item:< [%% $_extension:e$ ] >> -> Ploc.raise loc (Invalid_argument "class_sig_item-%%extension")

]
;

value rewrite_gc arg ((loc, ci, tyl, rto, attrs) : generic_constructor) maxpos = 
  let startpos = Ploc.last_pos loc in
  let l = comments_between (get arg) startpos maxpos in
  let l = Std.filter is_doc_comment l in
  let newattrs = List.map attr_doc_comment l in
  let attrs = <:vala< (uv attrs) @ newattrs >> in
  ((loc, ci, tyl, rto, attrs), Ploc.first_pos loc)
;

value rewrite_field arg ((loc, f, m, ty, attrs) : (loc * string * bool * ctyp * attributes)) maxpos = 
  let startpos = Ploc.last_pos loc in
  let l = comments_between (get arg) startpos maxpos in
  let l = Std.filter is_doc_comment l in
  let newattrs = List.map attr_doc_comment l in
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

value str_items_leading_doc_comments arg startpos (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:str_item< [@@@ $_attribute:_$ ] >>) -> ([str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], str_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  let l = floating@more_floating in
  match l with [
    [] -> ([], startpos)
  | _ -> (l, l |> sep_last |> fst |> fst |> loc_of_str_item |> Ploc.last_pos)
  ]
;

value sig_items_leading_doc_comments arg startpos (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map sig_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:sig_item< [@@@ $_attribute:_$ ] >>) -> ([sig_item_floating_attribute s], h2)
  | (Some s, _) -> ([], sig_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  let l = floating@more_floating in
  match l with [
    [] -> ([], startpos)
  | _ -> (l, l |> sep_last |> fst |> fst |> loc_of_sig_item |> Ploc.last_pos)
  ]
;

value str_item_preceding_doc_comment arg startpos (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:str_item< [@@@ $_attribute:_$ ] >>) -> ([str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], str_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  (floating@more_floating, (h2, loc2))
;

value rewrite_sig_item_pair arg (h1, loc1) (h2, loc2) =
  let startpos = Ploc.last_pos loc1 in
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (before, floating, after) = sig_item_apportion_interior_comments l in
  let floating = List.map sig_item_floating_attribute floating in
  let (more_floating2, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:sig_item< [@@@ $_attribute:_$ ] >>) -> ([sig_item_floating_attribute s], h2)
  | (Some s, _) -> ([], sig_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  let (more_floating1, h1) = match (before, h1) with [
    (None, _) -> ([], h1)
  | (Some s, <:sig_item< [@@@ $_attribute:_$ ] >>) -> ([sig_item_floating_attribute s], h1)
  | (Some s, _) -> ([], sig_item_wrap_itemattr (attr_doc_comment s) h1)
  ] in
  ((h1, loc1), more_floating1@floating@more_floating2, (h2, loc2))
;

value rewrite_class_sig_item_pair arg (h1, loc1) (h2, loc2) =
  let startpos = Ploc.last_pos loc1 in
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (before, floating, after) = sig_item_apportion_interior_comments l in
  let floating = List.map class_sig_item_floating_attribute floating in
  let (more_floating2, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:class_sig_item< [@@@ $_attribute:_$ ] >>) -> ([class_sig_item_floating_attribute s], h2)
  | (Some s, _) -> ([], class_sig_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  let (more_floating1, h1) = match (before, h1) with [
    (None, _) -> ([], h1)
  | (Some s, <:class_sig_item< [@@@ $_attribute:_$ ] >>) -> ([class_sig_item_floating_attribute s], h1)
  | (Some s, _) -> ([], class_sig_item_wrap_itemattr (attr_doc_comment s) h1)
  ] in
  ((h1, loc1), more_floating1@floating@more_floating2, (h2, loc2))
;

value rewrite_leading_sig_item arg startpos (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = sig_item_apportion_leading_comments l in
  let floating = List.map sig_item_floating_attribute floating in
  let (more_floating2, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:sig_item< [@@@ $_attribute:_$ ] >>) -> ([sig_item_floating_attribute s], h2)
  | (Some s, _) -> ([], sig_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  (floating@more_floating2, (h2, loc2))
;

value rewrite_leading_class_sig_item arg startpos (h2, loc2) =
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = sig_item_apportion_leading_comments l in
  let floating = List.map class_sig_item_floating_attribute floating in
  let (more_floating2, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:class_sig_item< [@@@ $_attribute:_$ ] >>) -> ([class_sig_item_floating_attribute s], h2)
  | (Some s, _) -> ([], class_sig_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  (floating@more_floating2, (h2, loc2))
;

value rewrite_class_str_item_pair arg startpos h2 =
  let loc2 = loc_of_class_str_item h2 in
  let bp2 = Ploc.first_pos loc2 in
  let l = comments_between (get arg) startpos bp2 in
  let (floating, after) = str_item_apportion_interior_comments l in
  let floating = List.map class_str_item_floating_attribute floating in
  let (more_floating, h2) = match (after, h2) with [
    (None, _) -> ([], h2)
  | (Some s, <:class_str_item< [@@@ $_attribute:_$ ] >>) -> ([class_str_item_floating_attribute s], h2)
  | (Some s, _) -> ([], class_str_item_wrap_itemattr (attr_doc_comment s) h2)
  ] in
  (floating@more_floating, h2)
;

value trailing_str_item_doc_comments arg spos epos =
  let l = comments_between (get arg) spos epos in
  let l = Std.filter is_doc_comment l in
  List.map str_item_floating_attribute l
;

value trailing_sig_item_doc_comments arg (h, loc) epos =
  let spos = Ploc.last_pos loc in
  let l = comments_between (get arg) spos epos in
  let (before, rest, after) = sig_item_apportion_interior_comments l in
  let (more_floating1, h) = match (before, h) with [
    (None, _) -> ([], h)
  | (Some s, <:sig_item< [@@@ $_attribute:_$ ] >>) -> ([s], h)
  | (Some s, _) -> ([], sig_item_wrap_itemattr (attr_doc_comment s) h)
  ] in

  let after = match after with [ None -> [] | Some x -> [x] ] in
  ((h, loc), List.map sig_item_floating_attribute (more_floating1@rest@after))
;

value trailing_class_sig_item_doc_comments arg (h, loc) epos =
  let spos = Ploc.last_pos loc in
  let l = comments_between (get arg) spos epos in
  let (before, rest, after) = sig_item_apportion_interior_comments l in
  let (more_floating1, h) = match (before, h) with [
    (None, _) -> ([], h)
  | (Some s, <:class_sig_item< [@@@ $_attribute:_$ ] >>) -> ([s], h)
  | (Some s, _) -> ([], class_sig_item_wrap_itemattr (attr_doc_comment s) h)
  ] in

  let after = match after with [ None -> [] | Some x -> [x] ] in
  ((h, loc), List.map class_sig_item_floating_attribute (more_floating1@rest@after))
;

value trailing_class_str_item_doc_comments arg spos epos =
  let l = comments_between (get arg) spos epos in
  let l = Std.filter is_doc_comment l in
  List.map class_str_item_floating_attribute l
;

value immediately_trailing_doc_comment arg hloc epos =
  match comments_between (get arg) (Ploc.last_pos hloc) epos with [
    [ s :: _ ] when is_doc_comment s -> Some s
  | [ s1 ; s2 :: _ ]
    when (not (is_comment s1))
      && (not (is_blank_line s1))
      && is_doc_comment s2 ->
      Some s2
  | _ -> None
  ]
;

value is_doc_attribute a = match uv a with [
  <:attribute_body< "ocaml.doc" $str:_$ ; >> -> True
| _ -> False
]
;

value variant_type_decl_add_doc_comment td s =
  match td.tdDef with [
    <:ctyp:< [ $list:l$ ] >> ->
      let (last, l) = sep_last l in
      let (loc, ci, tyl, rto, attrs) = last in
      let attrs = <:vala< (uv attrs) @ [ attr_doc_comment s ] >> in
      let last =  (loc, ci, tyl, rto, attrs) in
      let l = l @ [ last ] in
      { (td) with tdDef = <:ctyp:< [ $list:l$ ] >> }
  | _ -> assert False
  ]
;

value str_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) epos =
  match h1 with [
    <:str_item:< type $_flag:nrf$ $list:tdl$ >>
      when match (tdl |> sep_last |> fst).tdDef with [ <:ctyp< [ $list:_$ ] >> -> True | _ -> False ]
       &&  (tdl |> sep_last |> fst).tdAttributes
           |> uv
           |> List.for_all (fun a -> not (is_doc_attribute a)) ->
      match immediately_trailing_doc_comment arg h1loc epos with [
        None -> h1
      | Some s ->
        let (last, tdl) = sep_last tdl in
        let last = variant_type_decl_add_doc_comment last s in
        let tdl = tdl @ [ last ] in
        <:str_item< type $_flag:nrf$ $list:tdl$ >>
      ]

  | _ -> h1
  ]
;

value sig_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) epos =
  match h1 with [
    <:sig_item:< type $_flag:nrf$ $list:tdl$ >>
      when match (tdl |> sep_last |> fst).tdDef with [ <:ctyp< [ $list:_$ ] >> -> True | _ -> False ]
       &&  (tdl |> sep_last |> fst).tdAttributes
           |> uv
           |> List.for_all (fun a -> not (is_doc_attribute a)) ->
      match immediately_trailing_doc_comment arg h1loc epos with [
        None -> h1
      | Some s ->
        let (last, tdl) = sep_last tdl in
        let last = variant_type_decl_add_doc_comment last s in
        let tdl = tdl @ [ last ] in
        <:sig_item< type $_flag:nrf$ $list:tdl$ >>
      ]

  | _ -> h1
  ]
;

value extend_location arg hloc epos =
  let txtloc = match comments_between (get arg) (Ploc.last_pos hloc) epos with [
    [ s :: _ ] when is_doc_comment s -> fst s
  | [ s1 ; s2 :: _ ]
    when (not (is_comment s1))
      && (not (is_blank_line s1))
      && is_doc_comment s2 ->
      fst s2
  | _ -> hloc
  ] in
  Ploc.encl hloc txtloc
;

value extend_str_item_type_decl_location arg (h1, h1loc) epos =
  match h1 with [
    <:str_item< type $_flag:_$ $list:tdl$ >>
      when match (tdl |> sep_last |> fst).tdDef with [ <:ctyp< [ $list:_$ ] >> -> True | _ -> False ] ->
      extend_location arg h1loc epos
  | _ -> h1loc
  ]
;

value extend_type_decl_locations_in_str_items arg loc l =
  let rec erec = fun [
    [ (h1, h1loc) ; (h2, h2loc) :: t ] ->
    let h1 = str_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) (Ploc.first_pos h2loc) in
    [ (h1, h1loc) :: erec [ (h2, h2loc) :: t] ]
  | [ (h1, h1loc) ] ->
    let h1 = str_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) (Ploc.last_pos loc) in
    [ (h1, h1loc) ]
  | [] -> []
  ]
  in erec l
;

value extend_type_decl_locations_in_sig_items arg loc l =
  let rec erec = fun [
    [ (h1, h1loc) ; (h2, h2loc) :: t ] ->
    let h1 = sig_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) (Ploc.first_pos h2loc) in
    [ (h1, h1loc) :: erec [ (h2, h2loc) :: t] ]
  | [ (h1, h1loc) ] ->
    let h1 = sig_item_type_decl_adjust_last_doc_comment arg (h1, h1loc) (Ploc.last_pos loc) in
    [ (h1, h1loc) ]
  | [] -> []
  ]
  in erec l
;

value extend_sig_item_type_decl_location arg (h1, h1loc) epos =
  match h1 with [
    <:sig_item< type $_flag:_$ $list:tdl$ >>
      when match (tdl |> sep_last |> fst).tdDef with [ <:ctyp< [ $list:_$ ] >> -> True | _ -> False ] ->
      extend_location arg h1loc epos
  | _ -> h1loc
  ]
;

value extend_type_decl_locations_in_sig_items arg loc l =
  let rec erec = fun [
    [ (h1, h1loc) ; (h2, h2loc) :: t ] ->
    let h1loc = extend_sig_item_type_decl_location arg (h1, h1loc) (Ploc.first_pos h2loc) in
    [ (h1, h1loc) :: erec [ (h2, h2loc) :: t] ]
  | [ (h1, h1loc) ] ->
    let h1loc = extend_sig_item_type_decl_location arg (h1, h1loc) (Ploc.last_pos loc) in
    [ (h1, h1loc) ]
  | [] -> []
  ]
  in erec l
;

value rewrite_str_item arg = fun [
  (<:str_item:< type $_flag:nrfl$ $list:tdl$ >>, siloc) ->
  let tdl = rewrite_type_decls arg (Ploc.last_pos siloc) tdl in
  (<:str_item< type $_flag:nrfl$ $list:tdl$ >>, siloc)
| x -> x
]
;

value rewrite_sig_item arg = fun [
  (<:sig_item:< type $_flag:nrfl$ $list:tdl$ >>, siloc) ->
  let tdl = rewrite_type_decls arg (Ploc.last_pos siloc) tdl in
  (<:sig_item< type $_flag:nrfl$ $list:tdl$ >>, siloc)
| x -> x
]
;

value rewrite_implem0 arg loc l =
  let rec rerec startpos = fun [
    [ h1 :: t ] ->
    let (floating, h1) = str_item_preceding_doc_comment arg startpos h1 in
    floating @ [h1] @ (rerec (Ploc.last_pos (snd h1)) t)
  | [] ->
    (trailing_str_item_doc_comments arg startpos (Ploc.last_pos loc))
  ] in
  rerec (Ploc.first_pos loc) l
;

value rewrite_structure0 arg loc l =
  l
  |> List.map (rewrite_str_item arg)
  |> extend_type_decl_locations_in_str_items arg loc
  |> rewrite_implem0 arg loc
;

value rewrite_structure arg loc l =
  l
  |> List.map (fun si -> (si, loc_of_str_item si))
  |> rewrite_structure0 arg loc
  |> List.map fst
;

value rewrite_class_structure arg loc l =
  let rec rerec startpos = fun [
    [ h1  :: t ] ->
    let (floating, h1) = rewrite_class_str_item_pair arg startpos h1 in
    floating @ [h1] @ (rerec (Ploc.last_pos (loc_of_class_str_item h1)) t)
  | [] ->
    trailing_class_str_item_doc_comments arg startpos (Ploc.last_pos loc)
  ] in
  rerec (Ploc.first_pos loc) l
;

value rewrite_implem arg (sil, status) = 
  match sil with [
    [] -> ([], status)
  | [ h :: _ ] ->
    let (floating, startpos) = str_items_leading_doc_comments arg 0 h in

    (floating @ rewrite_structure0 arg (Ploc.make_unlined (startpos, max_int)) sil,
     status)
  ]
;

value lower_from_pairs f loc_fun l =
  let l = List.map (fun x -> (x, loc_fun x)) l in
  let (l, _) = f (l, None) in
  List.map fst l
;

value flatten_interf (l, status) =
  let rec frec acc = fun [
    [] -> List.rev acc
  | [ (<:sig_item< declare $list:l$ end >>,_) :: t ] ->
    let l = List.map (fun si -> (si, loc_of_sig_item si)) l in
      frec acc (l@t)
  | [ h :: t ] -> frec [h :: acc] t
  ]
  in (frec [] l, status)
;

value flatten_signature l = lower_from_pairs flatten_interf loc_of_sig_item l ;

value flatten_implem (l, status) =
  let rec frec acc = fun [
    [] -> List.rev acc
  | [ (<:str_item< declare $list:l$ end >>,_) :: t ] ->
    let l = List.map (fun si -> (si, loc_of_str_item si)) l in
      frec acc (l@t)
  | [ h :: t ] -> frec [h :: acc] t
  ]
  in (frec [] l, status)
;

value flatten_structure l = lower_from_pairs flatten_implem loc_of_str_item l ;

value flatten_class_signature l =
  let rec frec acc = fun [
    [] -> List.rev acc
  | [ <:class_sig_item< declare $list:l$ end >> :: t ] ->
      frec acc (l@t)
  | [ h :: t ] -> frec [h :: acc] t
  ]
  in frec [] l
;

value rewrite_interf0 arg loc l =
  let rec rerec = fun [
    [ h1 ; h2 :: t ] ->
    let (h1, floating, h2) = rewrite_sig_item_pair arg h1 h2 in
    [h1]@ floating @ (rerec [h2 :: t])
  | [h] ->
    let (h, trailing) = trailing_sig_item_doc_comments arg h (Ploc.last_pos loc) in
    [h]@trailing
  ] in
  let h1 = List.hd l in
  let (floating, h1) = rewrite_leading_sig_item arg (Ploc.first_pos loc) h1 in
  floating @ (rerec [h1 :: List.tl l])
;

value rewrite_signature0 arg loc l =
  l
  |> List.map (rewrite_sig_item arg)
  |> extend_type_decl_locations_in_sig_items arg loc
  |> rewrite_interf0 arg loc
;

value rewrite_signature arg loc l =
  l
  |> List.map (fun si -> (si, loc_of_sig_item si))
  |> rewrite_signature0 arg loc
  |> List.map fst
;

value rewrite_class_signature arg loc l =
  let rec rerec = fun [
    [ h1 ; h2 :: t ] ->
    let ((h1, _), floating, (h2, _)) = rewrite_class_sig_item_pair arg (h1, loc_of_class_sig_item h1) (h2, loc_of_class_sig_item h2) in
    [h1] @ floating @ (rerec [h2 :: t])
  | [h] ->
    let ((h, _), trailing) = trailing_class_sig_item_doc_comments arg (h, loc_of_class_sig_item h) (Ploc.last_pos loc) in
    [h] @ trailing
  ] in
  match l with [
    [] -> []
  | [ h1 :: _ ] ->
    let (floating, (h1, _)) = rewrite_leading_class_sig_item arg (Ploc.first_pos loc) (h1, loc_of_class_sig_item h1) in
    floating @ (rerec [h1 :: List.tl l])
  ]
;

value wrap_implem arg z = do {
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let fname = Ctxt.filename arg in
  let l = comments_of_file fname in
  let m = make_comment_map l in
  init arg m ;
  (sil, status)
}
;

value rewrite_interf arg (sil, status) = 
  match sil with [
    [] -> ([], status)
  | [ h :: _ ] ->
    let (floating, startpos) = sig_items_leading_doc_comments arg 0 h in

    (floating @ rewrite_signature0 arg (Ploc.make_unlined (startpos, max_int)) sil,
     status)
  ]
;

value wrap_interf arg z = do {
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let fname = Ctxt.filename arg in
  let l = comments_of_file fname in
  let m = make_comment_map l in
  init arg m ;
 (sil, status)
}
;

value install () = 
let ef = EF.mk () in 

let ef = EF.{ (ef) with
            class_expr = extfun ef.class_expr with [
    <:class_expr:< object $_opt:cspo$ $list:cf$ end >> ->
    fun arg ->
    let cf = rewrite_class_structure arg loc cf in
    Some (Pa_passthru.class_expr0 arg <:class_expr< object $_opt:cspo$ $list:cf$ end >>)
  ] } in

let ef = EF.{ (ef) with
            module_expr = extfun ef.module_expr with [
    <:module_expr:< struct $list:l$ end >> ->
    fun arg ->
    let l = l |> flatten_structure |> rewrite_structure arg loc in
    Some (Pa_passthru.module_expr0 arg <:module_expr:< struct $list:l$ end >>)
  ] } in

let ef = EF.{ (ef) with
            module_type = extfun ef.module_type with [
    <:module_type:< sig $list:l$ end >> ->
    fun arg ->
     let l = l |> flatten_signature |> rewrite_signature arg loc in
    Some (Pa_passthru.module_type0 arg <:module_type:< sig $list:l$ end >>)
  ] } in

let ef = EF.{ (ef) with
            class_type = extfun ef.class_type with [
    <:class_type:< object $_opt:cst$ $list:l$ end >> ->
    fun arg ->
     let l = l |> flatten_class_signature |> rewrite_class_signature arg loc in
    Some (Pa_passthru.class_type0 arg <:class_type:< object $_opt:cst$ $list:l$ end >>)
  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg -> 
      Some (z |> wrap_implem arg |> flatten_implem |> rewrite_implem arg |> Pa_passthru.implem0 arg)
  ] } in

let ef = EF.{ (ef) with
              interf = extfun ef.interf with [
    z ->
    fun arg -> 
      Some (z |> wrap_interf arg |> flatten_interf |> rewrite_interf arg |> Pa_passthru.interf0 arg)
  ] } in

  Pa_passthru.(install { name = "pa_dock_doc_comment" ; ef = ef ; pass = Some 0 ; before = [] ; after = [] })
;

install();
