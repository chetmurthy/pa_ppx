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

value comments_of_file f =
  let s = f |> Fpath.v |> Bos.OS.File.read
             |> Rresult.R.get_ok in
  comments_of_string s
;
