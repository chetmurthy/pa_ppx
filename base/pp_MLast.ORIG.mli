(* camlp5r *)
(* pp_MLast.ml,v *)

IFDEF BOOTSTRAP THEN

module Ploc : sig
include (module type of Ploc with type t = Ploc.t)

val pp_loc_verbose : bool ref
val pp : t Fmt.t
type 'a vala = [%import: 'a Ploc.vala] [@@deriving show]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

ELSE
val show_longid : MLast.longid -> string
val show_longid_lident : MLast.longid_lident -> string
val pp_longid_lident : MLast.longid_lident Fmt.t
val show_ctyp : MLast.ctyp -> string
val pp_ctyp : MLast.ctyp Fmt.t
val pp_attribute : MLast.attribute Fmt.t
END


