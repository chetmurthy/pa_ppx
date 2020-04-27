(* camlp5r *)
(* pa_inline_test.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_passthru ;
open Ppxutil ;

value libname = ref "" ;

value rewrite_str_item arg = fun [
  <:str_item:< [%%test value _ = $exp:e$ ; ] >>
| <:str_item:< [%%test $exp:e$ ; ] >>
 as z ->
  let descr = Printf.sprintf ": <<%s>>" (prettyprint_expr e) in
  let startpos = start_position_of_loc loc in
  let endpos = end_position_of_loc loc in
  let filename = startpos.Lexing.pos_fname in
  let line_number = string_of_int startpos.Lexing.pos_lnum in
  let start_pos = string_of_int (startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol) in
  let end_pos = string_of_int (endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol) in
  <:str_item< 
  Ppx_inline_test_lib.Runtime.test ~{config=(module Inline_test_config)}
    ~{descr = $str:descr$} ~{tags=[]} ~{filename = $str:filename$}
    ~{line_number= $int:line_number$} ~{start_pos = $int:start_pos$} ~{end_pos = $int:end_pos$}
     (fun () -> $exp:e$)
  >>
]
;

value wrap_implem arg z = do {
  if libname.val = "" then
    failwith "Must set -pa_inline_test-lib to a libname to use pa_inline_test"
  else  () ;
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let before_si = <:str_item< Ppx_inline_test_lib.Runtime.set_lib_and_partition $str:libname.val$ "" >> in
  let after_si = <:str_item< Ppx_inline_test_lib.Runtime.unset_lib $str:libname.val$ >> in
  ([(before_si, loc)]@sil@[(after_si, loc)], status)
}
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< [%%test value _ = $exp:_$ ; ] >>
  | <:str_item:< [%%test $exp:_$ ; ] >> as z ->
    fun arg ->
      Some (rewrite_str_item arg z)
  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg -> Some (wrap_implem arg (Pa_passthru.implem0 arg z))
  ] } in

  Pa_passthru.install ("pa_inline_test", ef)
;

install();

Pcaml.add_option "-pa_inline_test-lib" (Arg.Set_string libname)
  "<string> Name of library for this inline test module.";
