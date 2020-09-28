(* camlp5r *)
(* pp_MLast.ml,v *)

IFDEF BOOTSTRAP THEN

module Ploc = struct
include Ploc


let pp0_loc ppf loc =
  let fname = Ploc.file_name loc in
  let line = Ploc.line_nb loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  let bol = Ploc.bol_pos loc in

  let bp = bp - bol + 1 in
  let ep = ep - bol + 1 in
  Fmt.(pf ppf "<%a:%d:%d-%d>" (quote string) fname line bp ep)

let pp1_loc ppf x = Fmt.(const string "<loc>" ppf ())

let pp_loc_verbose = ref false

let pp ppf x =
  if !pp_loc_verbose then
    pp0_loc ppf x
  else
    pp1_loc ppf x

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

ELSE
let show_longid _ = "<longid>"
let show_longid_lident _ = "<longid_lident>"
let pp_longid_lident pps x = Fmt.(pf pps "<longid_lident>")
let show_ctyp _ = "<ctyp>"
let pp_ctyp pps x = Fmt.(pf pps "<ctyp>")
let pp_attribute pps x = Fmt.(pf pps "<attribute>")

END
