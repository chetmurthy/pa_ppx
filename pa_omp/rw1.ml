
module SRC = All_ast.Ast_4_02
module DST = All_ast.Ast_4_03

let _rewrite_list subrw0 __dst__ l =
  List.map (subrw0 __dst__) l

let rewrite_402_label_403_arg_label : 'a -> SRC.Asttypes.label -> DST.Asttypes.arg_label =
  fun __dst__ x ->
    if x <> "" then
      if x.[0] = '?' then DST.Asttypes.Optional (String.sub x 1 (String.length x - 1))
      else DST.Asttypes.Labelled x
    else
      DST.Asttypes.Nolabel

let rewrite_402_constant_403_constant :
  'a -> SRC.Asttypes.constant -> DST.Parsetree.constant =
  fun __dst__ -> function
  | SRC.Asttypes.Const_int x0 ->
      DST.Parsetree.Pconst_integer (string_of_int x0, None)
  | SRC.Asttypes.Const_char x0 ->
      DST.Parsetree.Pconst_char x0
  | SRC.Asttypes.Const_string (x0,x1) ->
      DST.Parsetree.Pconst_string
        (x0, x1)
  | SRC.Asttypes.Const_float x0 ->
      DST.Parsetree.Pconst_float (x0, None)
  | SRC.Asttypes.Const_int32 x0 ->
      DST.Parsetree.Pconst_integer (Int32.to_string x0, Some 'l')
  | SRC.Asttypes.Const_int64 x0 ->
      DST.Parsetree.Pconst_integer (Int64.to_string x0, Some 'L')
  | SRC.Asttypes.Const_nativeint x0 ->
      DST.Parsetree.Pconst_integer (Nativeint.to_string x0, Some 'n')

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

and label = [%import: All_ast.Ast_4_02.Asttypes.label
]

and closed_flag =  [%import: All_ast.Ast_4_02.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_02.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_02.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_02.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_02.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_02.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_02.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_02.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_02.Asttypes.constant]
and location_stack = [%import: All_ast.Ast_4_02.Parsetree.location_stack
  [@with Location.t := location_t]
]

[@@deriving rewrite
    { dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
    ; dispatchers = {
        rewrite_string_option = {
          srctype = [%typ: string option]
        ; dsttype = [%typ: string option]
        ; code = (fun _ x -> x)
        }
        ; rewrite_Lexing_position = {
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
      ; rewrite_label = {
          srctype = [%typ: label]
        ; dsttype = [%typ: DST.Asttypes.arg_label]
        ; code = rewrite_402_label_403_arg_label
        }
      ; rewrite_closed_flag = {
          srctype = [%typ: closed_flag]
        ; dsttype = [%typ: DST.Asttypes.closed_flag]
        }
      ; rewrite_direction_flag = {
          srctype = [%typ: direction_flag]
        ; dsttype = [%typ: DST.Asttypes.direction_flag]
        }
      ; rewrite_private_flag = {
          srctype = [%typ: private_flag]
        ; dsttype = [%typ: DST.Asttypes.private_flag]
        }
      ; rewrite_mutable_flag = {
          srctype = [%typ: mutable_flag]
        ; dsttype = [%typ: DST.Asttypes.mutable_flag]
        }
      ; rewrite_virtual_flag = {
          srctype = [%typ: virtual_flag]
        ; dsttype = [%typ: DST.Asttypes.virtual_flag]
        }
      ; rewrite_override_flag = {
          srctype = [%typ: override_flag]
        ; dsttype = [%typ: DST.Asttypes.override_flag]
        }
      ; rewrite_variance = {
          srctype = [%typ: variance]
        ; dsttype = [%typ: DST.Asttypes.variance]
        }
      ; rewrite_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.Parsetree.constant]
        ; code = rewrite_402_constant_403_constant
        }
      ; rewrite_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _rewrite_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_location_stack = {
          srctype = [%typ: location_stack]
        ; dsttype = [%typ: DST.Parsetree.location_stack]
        }
      }
    }
]
