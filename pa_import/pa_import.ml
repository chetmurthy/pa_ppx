(* camlp5r *)
(* pa_import.ml,v *)
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

value lookup_path = ref [] ;

value add_package s =
  let l = String.split_on_char ',' s in
  let l = filter (fun s -> s <> "") l in
  push lookup_path (Left l)
;

value add_include s =
  push lookup_path (Right s)
;

value import_type loc arg t = do {
  let path = lookup_path.val in
  let path = List.map (fun [ Left l -> String.concat "," l | Right s -> s ]) path in
  let path = String.concat ":" path in
  Fmt.(pf stderr "[import_type: packages: %s]\n%!" path) ;
  <:ctyp< int >> 
}
;

value registered_ctyp_extension arg = fun [
  <:ctyp:< [% import: $type:t$ ] >> ->
    Some (import_type loc arg t)
| _ -> assert False
]
;

value install_to_ef ef =

let ef = EF.{ (ef) with
  ctyp = extfun ef.ctyp with [
    <:ctyp:< [% import: $type:_$ ] >> as z ->
      fun arg ->
        registered_ctyp_extension arg z
  ] } in
ef
;

Pcaml.add_option "-pa_import-package" (Arg.String add_package)
  "<string> list of packages to search for CMI files.";

Pcaml.add_option "-pa_import-I" (Arg.String add_include)
  "<string> include-directory to search for CMI files.";
