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

value quote_string_list loc l =
  List.fold_right (fun s rhs -> <:expr< [ $str:s$ :: $rhs$ ] >>) l <:expr< [] >>
;

value bool_test arg tags loc descr e =
  let startpos = start_position_of_loc loc in
  let endpos = end_position_of_loc loc in
  let filename = startpos.Lexing.pos_fname in
  let line_number = string_of_int startpos.Lexing.pos_lnum in
  let start_pos = string_of_int (startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol) in
  let end_pos = string_of_int (endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol) in
  <:str_item< 
  Ppx_inline_test_lib.Runtime.test ~{config=(module Inline_test_config)}
    ~{descr = $str:descr$} ~{tags = $quote_string_list loc tags$} ~{filename = $str:filename$}
    ~{line_number= $int:line_number$} ~{start_pos = $int:start_pos$} ~{end_pos = $int:end_pos$}
     (fun () -> $exp:e$)
  >>
;

value unit_test arg tags loc descr e =
  let startpos = start_position_of_loc loc in
  let endpos = end_position_of_loc loc in
  let filename = startpos.Lexing.pos_fname in
  let line_number = string_of_int startpos.Lexing.pos_lnum in
  let start_pos = string_of_int (startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol) in
  let end_pos = string_of_int (endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol) in
  <:str_item< 
  Ppx_inline_test_lib.Runtime.test_unit ~{config=(module Inline_test_config)}
    ~{descr = $str:descr$} ~{tags = $quote_string_list loc tags$} ~{filename = $str:filename$}
    ~{line_number= $int:line_number$} ~{start_pos = $int:start_pos$} ~{end_pos = $int:end_pos$}
     (fun () -> $exp:e$)
  >>
;

value module_test arg tags loc descr me =
  let startpos = start_position_of_loc loc in
  let endpos = end_position_of_loc loc in
  let filename = startpos.Lexing.pos_fname in
  let line_number = string_of_int startpos.Lexing.pos_lnum in
  let start_pos = string_of_int (startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol) in
  let end_pos = string_of_int (endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol) in
  <:str_item< 
  Ppx_inline_test_lib.Runtime.test_module ~{config=(module Inline_test_config)}
    ~{descr = $str:descr$} ~{tags = $quote_string_list loc tags$} ~{filename = $str:filename$}
    ~{line_number= $int:line_number$} ~{start_pos = $int:start_pos$} ~{end_pos = $int:end_pos$}
     (fun () -> let module M = $mexp:me$ in ())
  >>
;

value extract_tags attr =
 let l = match Pcaml.unvala attr with [
   <:attribute_body< tags ( $list:l$ ) ; >> -> l
 | <:attribute_body< tags $exp:e$ ; >> -> [e]
 | _ -> failwith "pa_inline_test.extract_tags: bad attribute"
 ] in
 List.map (fun [ <:expr< $str:s$ >> -> s | _ -> failwith "extract_tags: bad tag" ]) l
;

value attrs_to_tags = fun [
  [] -> []
| [a] -> extract_tags a
| _ -> failwith "pa_inline_test.rewrite_str_item: at most one attr allowed"
]
;

value rewrite_str_item arg z =
  match Pa_passthru.str_item0 arg z with [
  <:str_item:< [%%test $exp:e$ ; ] >>
  ->
  let descr = Printf.sprintf ": <<%s>>" (prettyprint_expr e) in
  bool_test arg [] loc descr e

| <:str_item:< [%%test_unit $exp:e$ ; ] >>
  ->
  let descr = Printf.sprintf ": <<%s>>" (prettyprint_expr e) in
  unit_test arg [] loc descr e

| <:str_item:< [%%test_module (module $mexp:me$) ; ] >> ->
  module_test arg [] loc "" me

| <:str_item:< [%%test value $flag:False$ $list:[(p,e,_)]$ ; ] >> ->
  let (p, attrs) = Patt.unwrap_attrs p in
  let tags = attrs_to_tags attrs in
  let descr = match p with [
    <:patt< _ >> -> Printf.sprintf ": <<%s>>" (prettyprint_expr e)
  | <:patt< $str:descr$ >> -> ": "^descr
  | _ -> failwith "pa_inline_test.rewrite_str_item: bad lhs of let"
  ] in
  bool_test arg tags loc descr e

| <:str_item:< [%%test_unit value $flag:False$ $list:[(p,e,_)]$ ; ] >> ->
  let (p, attrs) = Patt.unwrap_attrs p in
  let tags = attrs_to_tags attrs in
  let descr = match p with [
    <:patt< _ >> -> Printf.sprintf ": <<%s>>" (prettyprint_expr e)
  | <:patt< $str:descr$ >> -> ": "^descr
  | _ -> failwith "pa_inline_test.rewrite_str_item: bad lhs of let"
  ] in
  unit_test arg tags loc descr e

| <:str_item:< [%%test_module value $flag:False$ $list:[(p,e,_)]$ ; ] >> ->
  let (p, attrs) = Patt.unwrap_attrs p in
  let tags = attrs_to_tags attrs in
  let descr = match p with [
    <:patt< _ >> -> Printf.sprintf ": <<%s>>" (prettyprint_expr e)
  | <:patt< $str:descr$ >> -> ": "^descr
  | _ -> failwith "pa_inline_test.rewrite_str_item: bad lhs of let"
  ] in
  let me = match e with [
    <:expr< (module $mexp:me$) >> -> me
  | _ -> failwith "module_test without module payload" ] in
  module_test arg tags loc descr me
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

value is_string_patt = fun [ <:patt< $str:_$ >> -> True | _ -> False ] ;
value is_unnamed_patt = fun [ <:patt< _ >> -> True | _ -> False ] ;

value is_patt_bool_test = fun [
  <:str_item:< [%%test value $flag:False$ $list:[(p,_,_)]$ ; ] >>
  when is_string_patt (fst (Patt.unwrap_attrs p)) ||  is_unnamed_patt (fst (Patt.unwrap_attrs p)) -> True
| _ -> False
]
;

value is_patt_unit_test = fun [
  <:str_item:< [%%test_unit value $flag:False$ $list:[(p,_,_)]$ ; ] >>
  when is_string_patt (fst (Patt.unwrap_attrs p)) ||  is_unnamed_patt (fst (Patt.unwrap_attrs p)) -> True
| _ -> False
]
;

value is_patt_module_test = fun [
  <:str_item:< [%%test_module value $flag:False$ $list:[(p,_,_)]$ ; ] >>
  when is_string_patt (fst (Patt.unwrap_attrs p)) ||  is_unnamed_patt (fst (Patt.unwrap_attrs p)) -> True
| _ -> False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< [%%test $exp:_$ ; ] >> as z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%test_unit $exp:_$ ; ] >> as z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%test_module (module $mexp:_$) ; ] >> as z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%test value $flag:False$ $list:_$ ; ] >>
   as z when is_patt_bool_test z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%test_unit value $flag:False$ $list:_$ ; ] >>
   as z when is_patt_unit_test z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%test_module value $flag:False$ $list:_$ ; ] >>
   as z when is_patt_module_test z ->
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
