(* camlp5r *)
(* pa_expect_test.ml,v *)
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

module ExpectContext = struct
  type t = { md5 : string } ;
    
type scratchdata_t += [ Pa_expect_test of t ] ;

value get arg =
  match Ctxt.scratchdata arg "expect_test" with [
    Pa_expect_test c -> c
  | _ -> assert False
  ]
;
value init arg md5 =
   Ctxt.init_scratchdata arg "expect_test" (Pa_expect_test { md5 = md5 })
;
end
;
module EC = ExpectContext ;

value expect_location loc =
  let filename = Ploc.file_name loc in
  let line_number = Ploc.line_nb loc in
  let line_start = Ploc.bol_pos loc in
  let start_pos = Ploc.first_pos loc in
  let end_pos = Ploc.last_pos loc in
  <:expr< {
    filename =
          (Expect_test_common.Std.File.Name.of_string
           $str:filename$);
    line_number = $int:string_of_int line_number$;
    line_start = $int:string_of_int line_start$;
    start_pos = $int:string_of_int start_pos$;
    end_pos = $int:string_of_int end_pos$
  } >>
;

value expect_test arg extension_loc attrid_loc (descr_loc, descr) test_e expect_e =
  let (expect_loc, expectid_loc, expect_s_loc, expect_s) = match expect_e with [
    <:expr:< [% $attrid:(expectid_loc, "expect")$ $exp:paye$ ; ] >> ->
      let (sloc, s) = match paye with [ <:expr:< $str:s$ >> -> (loc, s) ] in
      (loc, expectid_loc, sloc, s)
  ] in
  let digest = (EC.get arg).EC.md5 in
  let filename = Ctxt.filename arg in
  let q_descr =
    let loc = descr_loc in
    match descr with [ None -> <:expr< None >> | Some s -> <:expr< Some $str:s$ >> ] in
  let loc = extension_loc in
  <:str_item<
  let module Expect_test_collector =
    (Expect_test_collector.Make)(Expect_test_config) in
    Expect_test_collector.run
      ~{file_digest = (Expect_test_common.Std.File.Digest.of_string $str:digest$)}
      ~{location = $expect_location loc$}
      ~{absolute_filename = $str:filename$}
      ~{description = $q_descr$}
      ~{tags = []}
      ~{expectations = [({
                        tag = (Some "");
                        body = (Pretty $str:expect_s$);
                        extid_location = $expect_location expectid_loc$;
                        body_location = $expect_location expect_s_loc$
                      } : Expect_test_common.Std.Expectation.t string)]}
      ~{uncaught_exn_expectation = None} ~{inline_test_config = (module Inline_test_config)}
      (fun () -> do {
         $test_e$;
         Expect_test_collector.save_output $expect_location expectid_loc$
           })
  >>
;

value is_string_patt = fun [ <:patt< $str:_$ >> -> True | _ -> False ] ;
value is_expect_expr = fun [ <:expr< do { $_$ ; [%expect $str:_$ ; ] } >> -> True | _ -> False ] ;

value is_named_expect_test = fun [
  <:str_item:< [%%expect_test value $flag:False$ $list:[(p,e,_)]$ ; ] >>
  when is_string_patt p && is_expect_expr e -> True

| _ -> False
]
;

value rewrite_str_item arg = fun [
  <:str_item:< [%%expect_test do { $exp:e$ ; [%expect $str:expect$ ; ] } ; ] >> as z ->
  let (extension_loc, extension) = match z with [
    <:str_item:< [%% $extension:e$ ] >> -> (loc, e)
  ] in
  let attrid_loc = match extension with [
    <:attribute_body< $attrid:(loc,_)$ $structure:_$ >> -> loc
  ] in
  let rhs = match z with [
    <:str_item:< [%%expect_test $exp:rhs$ ; ] >> -> rhs
  ] in
  let (test_e, expect_e) = match z with [
    <:str_item:< [%%expect_test do { $exp:test_e$ ; $expect_e$ } ; ] >> -> (test_e, expect_e)
  ] in

  expect_test arg extension_loc attrid_loc (loc_of_expr rhs, None) test_e expect_e

  | <:str_item:< [%%expect_test value $flag:False$ $list:[(p,rhs,_)]$ ; ] >> as z -> do {
    assert (is_named_expect_test z) ;
  let descr = match p with [
    <:patt:< _ >> -> (loc, None)
  | <:patt:< $str:descr$ >> -> (loc, Some descr)
  | _ -> failwith "pa_expect_test.rewrite_str_item: bad lhs of let"
  ] in

  let (extension_loc, extension) = match z with [
    <:str_item:< [%% $extension:e$ ] >> -> (loc, e)
  ] in
  let attrid_loc = match extension with [
    <:attribute_body< $attrid:(loc,_)$ $structure:_$ >> -> loc
  ] in

  let (test_e, expect_e) = match rhs with [
    <:expr:< do { $exp:test_e$ ; $expect_e$ } >> -> (test_e, expect_e)
  ] in

    expect_test arg extension_loc attrid_loc descr test_e expect_e
  }
]
;

value wrap_implem arg z = do {
  let (sil, status) = z in
  let loc = sil |> List.hd |> snd in
  let fname = Ctxt.filename arg in
  let before_si = <:str_item< Expect_test_collector.Current_file.set
    ~{absolute_filename = $str:fname$ } >> in
  let after_si = <:str_item< Expect_test_collector.Current_file.unset () >> in
  ([(before_si, loc)]@sil@[(after_si, loc)], status)
}
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            str_item = extfun ef.str_item with [
    <:str_item:< [%%expect_test do { $exp:_$ ; [%expect $str:_$ ; ] } ; ] >> as z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  | <:str_item:< [%%expect_test value $flag:False$ $list:_$ ; ] >>
   as z when is_named_expect_test z ->
    fun arg ->
      Some (rewrite_str_item arg z)

  ] } in

let ef = EF.{ (ef) with
              implem = extfun ef.implem with [
    z ->
    fun arg -> 
      let md5 = arg |> Ctxt.filename |> Digest.file |> Digest.to_hex in
      let arg = do { EC.init arg md5 ; arg } in
      Some (wrap_implem arg (Pa_passthru.implem0 arg z))
  ] } in

  Pa_passthru.install ("pa_expect_test", ef)
;

install();
