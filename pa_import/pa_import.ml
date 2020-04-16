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

value packages = ref [] ;

value add_packages s =
  let l = String.split_on_char ',' s in
  List.iter (fun p -> if p <> "" then push packages p else ()) l
;

value import_type loc arg t = do {
  Fmt.(pf stderr "[import_type: packages: %s]\n%!" (String.concat "," packages.val)) ;
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

Pcaml.add_option "-pa_import-package" (Arg.String add_packages)
  "<string> list of packages to search for CMI files.";
