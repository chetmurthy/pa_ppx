(* camlp5r *)
(* testutil.ml,v *)

open Printf;

value with_input_file fname f arg =
  let oinput_file = Pcaml.input_file.val in do {
    Pcaml.input_file.val := fname ;
    try let rv = f arg in do { Pcaml.input_file.val := oinput_file ; rv }
    with exc -> do {
      Pcaml.input_file.val := oinput_file ;
      raise exc
    }
  }
;

module PAPR = struct
module Implem = struct
value pa ?{input_file="-"} strm = let (ast, _) = with_input_file input_file Pcaml.parse_implem.val strm in ast ;
value pa1 ?{input_file="-"} s = let ast = pa ~{input_file=input_file} (Stream.of_string s) in ast ;
value pa_all s =
  let strm = Stream.of_string s in
  let rec pall = parser [
    [: x = pa ; strm :] ->
    if x = [] then [] else
      x @ (pall strm)
  | [: :] -> [] ] in
  pall strm
;

value pr l = do {
  let sep = match Pcaml.inter_phrases.val with [ None -> "" | Some s -> s ] in
  let b = Buffer.create 23 in
    List.iter (fun (ast, _) -> 
      let s = Eprinter.apply Pcaml.pr_str_item Pprintf.empty_pc ast in do {
        Buffer.add_string b s ;
        Buffer.add_string b sep ;
      }) l ;
    Buffer.contents b
}
;
end;

module Interf = struct
value pa ?{input_file="-"} strm = let (ast, _) = with_input_file input_file Pcaml.parse_interf.val strm in ast ;
value pa1 ?{input_file="-"} s = let ast = pa ~{input_file=input_file} (Stream.of_string s) in ast ;
value pa_all s =
  let strm = Stream.of_string s in
  let rec pall = parser [
    [: x = pa ; strm :] ->
    if x = [] then [] else
      x @ (pall strm)
  | [: :] -> [] ] in
  pall strm
;

value pr l = do {
  let sep = match Pcaml.inter_phrases.val with [ None -> "" | Some s -> s ] in
  let b = Buffer.create 23 in
    List.iter (fun (ast, _) -> 
      let s = Eprinter.apply Pcaml.pr_sig_item Pprintf.empty_pc ast in do {
        Buffer.add_string b s ;
        Buffer.add_string b sep ;
      }) l ;
    Buffer.contents b
}
;
end;
value both_pa1 = (Implem.pa1, Interf.pa1) ;
value both_pr = (Implem.pr, Interf.pr) ;
end;

module Official = struct

module Implem = struct
value pa s =
  let lb = Lexing.from_string s in
  Parse.implementation lb
;
value pr st =
  Pprintast.string_of_structure st
;
end ;

module Interf = struct
value pa s =
  let lb = Lexing.from_string s in
  Parse.interface lb
;

value with_buffer_formatter f arg = do {
  let b = Buffer.create 23 in
  let bfmt = Format.formatter_of_buffer b in
  f bfmt arg ;
  Format.pp_print_flush bfmt () ;
  Buffer.contents b
}
;

value pr st =
  with_buffer_formatter Pprintast.signature st
;
end ;
value both_pa = (Implem.pa, Interf.pa) ;
value both_pr = (Implem.pr, Interf.pr) ;
end ;



(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
