
module SRC = All_ast.Ast_4_02
module DST = All_ast.Ast_4_03

let lexing_position_402_403 (p : SRC.Lexing.position) =
  DST.Lexing.{
    pos_fname = p.pos_fname;
    pos_lnum = p.pos_lnum;
    pos_bol = p.pos_bol;
    pos_cnum = p.pos_cnum }

let rec longident_t_402_403 ( p : SRC.Longident.t ) =
  match p with
    SRC.Longident.Lident s -> DST.Longident.Lident s
  | SRC.Longident.Ldot (p, s) -> DST.Longident.Ldot(longident_t_402_403 p, s)
  | SRC.Longident.Lapply (p1, p2) -> DST.Longident.Lapply(longident_t_402_403 p1, longident_t_402_403 p2)

type lexing_position = [%import: All_ast.Ast_4_02.Lexing.position]
and location_t = [%import: All_ast.Ast_4_02.Location.t
  [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_02.Location.loc
  [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_02.Longident.t
  [@with t := longident_t]
]

[@@deriving rewrite
    { dispatch_type = dispatch_table_t
    ; dispatchers = {
        rewrite_Lexing_position = {
          srctype = [%typ: lexing_position]
        ; dsttype = [%typ: DST.Lexing.position]
        }
      ; rewrite_Location_t = {
          srctype = [%typ: location_t]
        ; dsttype = [%typ: DST.Location.t]
        }
      ; rewrite_Location_loc = {
          srctype = [%typ: 'a location_loc]
        ; dsttype = [%typ: 'b DST.Location.loc]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_Longident_t = {
          srctype = [%typ: longident_t]
        ; dsttype = [%typ: DST.Longident.t]
        }
      }
    }
]
(*
type ('a, 'b) rewriter_t = dispatch_table_t -> 'a -> 'b

and dispatch_table_t = {
  rewrite_Lexing_position : (lexing_position, DST.Lexing.position) rewriter_t
; rewrite_Location_t : (location_t, DST.Location.t) rewriter_t
; rewrite_Location_loc : 'a 'b . ('a, 'b) rewriter_t -> ('a location_loc, 'b DST.Location.loc) rewriter_t
; rewrite_Longident_t : (longident_t, DST.Longident.t) rewriter_t
}
*)
let rewrite_Lexing_position (dt : dispatch_table_t) (p : lexing_position) : DST.Lexing.position =
  DST.Lexing.{
    pos_fname = p.pos_fname;
    pos_lnum = p.pos_lnum;
    pos_bol = p.pos_bol;
    pos_cnum = p.pos_cnum }

let rewrite_Location_t  (dt : dispatch_table_t) (p : location_t) : DST.Location.t =
  DST.Location.{
    loc_start = dt.rewrite_Lexing_position dt p.loc_start;
    loc_end = dt.rewrite_Lexing_position dt p.loc_end;
    loc_ghost = p.loc_ghost
  }

let rewrite_Location_loc : 'a 'b . ('a, 'b) rewriter_t -> ('a location_loc, 'b DST.Location.loc) rewriter_t =
  fun sub1 dt p ->
    DST.Location.{
      txt = sub1 dt p.txt ;
      loc = dt.rewrite_Location_t dt p.loc
    }

let rewrite_Longident_t (dt : dispatch_table_t) (p : longident_t) : DST.Longident.t =
  match p with
    Lident s -> DST.Longident.Lident s
  | Ldot (p, s) -> DST.Longident.Ldot(dt.rewrite_Longident_t dt p, s)
  | Lapply (p1, p2) -> DST.Longident.Lapply(dt.rewrite_Longident_t dt p1, dt.rewrite_Longident_t dt p2)

let dt = {
  rewrite_Lexing_position
; rewrite_Location_t
; rewrite_Location_loc
; rewrite_Longident_t
}
