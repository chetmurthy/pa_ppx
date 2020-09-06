
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
and attribute = [%import: All_ast.Ast_4_02.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_02.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_02.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_02.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_02.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_02.Parsetree.core_type_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.closed_flag := closed_flag
    ]
]
and package_type = [%import: All_ast.Ast_4_02.Parsetree.package_type
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_02.Parsetree.row_field]
and pattern = [%import: All_ast.Ast_4_02.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_02.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.constant := constant ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_02.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_02.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
      Asttypes.constant := constant
    ]
]
and case = [%import: All_ast.Ast_4_02.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_02.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_02.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_02.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_02.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_02.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_extension = [%import: All_ast.Ast_4_02.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_02.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_02.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
    ]
]
and class_signature = [%import: All_ast.Ast_4_02.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_02.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_02.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_02.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
    ]
]
and class_structure = [%import: All_ast.Ast_4_02.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_02.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_02.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_02.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_02.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_02.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_02.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_02.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_02.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_02.Parsetree.signature_item_desc]
and module_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_02.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_02.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_02.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_02.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_02.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_02.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_02.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_02.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_02.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_02.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_02.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_02.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_02.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
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
