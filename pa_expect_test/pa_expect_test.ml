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
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

module ExpectContext = struct
  type t = { md5 : string } ;
    
type scratchdata_t += [ Pa_expect_test of t ] ;

value get arg =
  match Ctxt.refscratchdata arg "expect_test" with [
    Pa_expect_test c -> c
  | _ -> assert False
  ]
;
value init arg md5 =
   Ctxt.init_refscratchdata arg "expect_test" (Pa_expect_test { md5 = md5 })
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

value transform_body e =
  let expectations = ref [] in
  let rec trec = fun [
    <:expr:< do { $list:l$ } >> ->
      <:expr< do { $list:List.map trec l$ } >>

  | <:expr:< [% $attrid:(expectid_loc, "expect")$ $exp:paye$ ; ] >> ->
    let (sloc, s) = match paye with [ <:expr:< $str:s$ >> -> (loc, s) ] in do {
    Std.push expectations (expectid_loc, sloc, <:expr< (Pretty $str:s$) >>) ;
    <:expr< Expect_test_collector.save_output $expect_location expectid_loc$ >>
  }

  | <:expr:< [% $attrid:(expectid_loc, "expect.output")$ ] >> -> do {
    Std.push expectations (expectid_loc, expectid_loc, <:expr< Output >>) ;
    <:expr< Expect_test_collector.save_and_return_output $expect_location expectid_loc$ >>
  }

  | <:expr:< [% $attrid:(expectid_loc, "expect.unreachable")$ ] >> -> do {
    Std.push expectations (expectid_loc, expectid_loc, <:expr< Unreachable >>) ;
    <:expr< Expect_test_collector.save_output $expect_location expectid_loc$ >>
  }

  | <:expr:< let $_flag:r$ $_list:l$ in $body$ >> ->
    let l = vala_map (List.map (fun (p,e,a) -> (p, trec e, a))) l in
    <:expr< let $_flag:r$ $_list:l$ in $trec body$ >>

  | <:expr:< if $e1$ then $e2$ else $e3$ >> ->
    let e1 = trec e1 in
    let e2 = trec e2 in
    let e3 = trec e3 in
    <:expr:< if $e1$ then $e2$ else $e3$ >>

  | e -> e
  ] in
  let te = trec e in
  (te, expectations.val)
;

value extract_uncaught_exn attr = match uv attr with [
  <:attribute_body:< $attrid:(expectid_loc, "expect.uncaught_exn")$ $exp:paye$ ; >> ->
    let (loc, s) = match paye with [ <:expr:< $str:s$ >> -> (loc, s) ] in
    Some (expectid_loc, loc, <:expr< (Pretty $str:s$) >>)
| _ -> None
]
;

value expect_test arg extension_loc attrid_loc (descr_loc, descr) test_body attrs =
  let (transformed_body, expectations) = transform_body test_body in

  let digest = (EC.get arg).EC.md5 in
  let filename = Ctxt.filename arg in
  let q_descr =
    let loc = descr_loc in
    match descr with [ None -> <:expr< None >> | Some s -> <:expr< Some $str:s$ >> ] in
  let loc = extension_loc in

  let expectations_list = List.fold_right (fun (expectid_loc, expect_s_loc, expected) rhs ->
      <:expr< [ ({
                        tag = (Some "");
                        body = $expected$;
                        extid_location = $expect_location expectid_loc$;
                        body_location = $expect_location expect_s_loc$
                      } : Expect_test_common.Std.Expectation.t string ) :: $rhs$ ] >>)
      expectations <:expr< [] >> in
  let uncaught = List.find_map extract_uncaught_exn attrs in
  let uncaught = match uncaught with [
    None -> <:expr< None >> 
  | Some (expectid_loc, expect_s_loc, expected) ->
    <:expr< Some ({
                        tag = (Some "");
                        body = $expected$;
                        extid_location = $expect_location expectid_loc$;
                        body_location = $expect_location expect_s_loc$
                      } : Expect_test_common.Std.Expectation.t string ) >>
  ] in

  <:str_item<
  let module Expect_test_collector =
    (Expect_test_collector.Make)(Expect_test_config) in
    Expect_test_collector.run
      ~{file_digest = (Expect_test_common.Std.File.Digest.of_string $str:digest$)}
      ~{location = $expect_location loc$}
      ~{absolute_filename = $str:filename$}
      ~{description = $q_descr$}
      ~{tags = []}
      ~{expectations = $expectations_list$}
      ~{uncaught_exn_expectation = $uncaught$} ~{inline_test_config = (module Inline_test_config)}
      (fun () -> $transformed_body$)
  >>
;

value is_string_patt = fun [ <:patt< $str:_$ >> -> True | _ -> False ] ;

value is_named_expect_test = fun [
  <:str_item:< [%%expect_test value $flag:False$ $list:[(p,_,_)]$ ; ] >>
  when is_string_patt p -> True

| _ -> False
]
;

value rewrite_str_item arg = fun [
  <:str_item:< [%%expect_test $exp:rhs$ $itemattrs:attrs$; ] >> as z ->
  let (extension_loc, extension) = match z with [
    <:str_item:< [%% $extension:e$ ] >> -> (loc, e)
  ] in
  let attrid_loc = match extension with [
    <:attribute_body< $attrid:(loc,_)$ $structure:_$ >> -> loc
  ] in

  expect_test arg extension_loc attrid_loc (loc_of_expr rhs, None) rhs attrs

  | <:str_item:< [%%expect_test value $flag:False$ $list:[(p,rhs,ia)]$ ; ] >> as z -> do {
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

    expect_test arg extension_loc attrid_loc descr rhs (uv ia)
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
    <:str_item:< [%%expect_test $exp:_$ $itemattrs:_$ ; ] >> as z ->
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

  Pa_passthru.(install { name = "pa_expect_test" ; ef = ef ; pass = None ; before = [] ; after = ["pa_inline_test"] })
;

install();
