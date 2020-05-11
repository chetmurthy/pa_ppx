(* camlp5r *)
(* base.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "q_MLast.cmo";

open MLast ;

value runtime_module_path = ref "Pa_ppx_runtime" ;

Pcaml.add_option "-pa_ppx-runtime" (Arg.Set_string runtime_module_path )
  "<string> full path to Pa_ppx_runtime module [for internal use only].";

value module_expr_runtime_module m =
  if runtime_module_path.val = "" then m
  else
    let loc = loc_of_module_expr m in
    <:module_expr< $uid:runtime_module_path.val$ . $m$ >>
;

value expr_runtime_module m =
  if runtime_module_path.val = "" then m
  else
    let loc = loc_of_expr m in
    <:expr< $uid:runtime_module_path.val$ . $m$ >>
;
