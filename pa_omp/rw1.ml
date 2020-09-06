
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
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_02.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_02.Parsetree.row_field
    [@with
      Asttypes.label := label
    ]
]
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
        rewrite_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
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
        ; dsttype = [%typ: DST.Asttypes.label]
        }
      ; rewrite_closed_flag = {
          srctype = [%typ: closed_flag]
        ; dsttype = [%typ: DST.Asttypes.closed_flag]
        }
      ; rewrite_rec_flag = {
          srctype = [%typ: rec_flag]
        ; dsttype = [%typ: DST.Asttypes.rec_flag]
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
      ; rewrite_attribute = {
          srctype = [%typ: attribute]
        ; dsttype = [%typ: DST.Parsetree.attribute]
        }
      ; rewrite_extension = {
          srctype = [%typ: extension]
        ; dsttype = [%typ: DST.Parsetree.extension]
        }
      ; rewrite_attributes = {
          srctype = [%typ: attributes]
        ; dsttype = [%typ: DST.Parsetree.attributes]
        }
      ; rewrite_payload = {
          srctype = [%typ: payload]
        ; dsttype = [%typ: DST.Parsetree.payload]
        }
      ; rewrite_core_type = {
          srctype = [%typ: core_type]
        ; dsttype = [%typ: DST.Parsetree.core_type]
        }
      ; rewrite_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.Parsetree.core_type_desc]
        }
      ; rewrite_package_type = {
          srctype = [%typ: package_type]
        ; dsttype = [%typ: DST.Parsetree.package_type]
        }
      ; rewrite_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.Parsetree.row_field]
        }
      ; rewrite_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.Parsetree.pattern]
        }
      ; rewrite_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.Parsetree.pattern_desc]
        }
      ; rewrite_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.Parsetree.expression]
        }
      ; rewrite_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        }
      ; rewrite_case = {
          srctype = [%typ: case]
        ; dsttype = [%typ: DST.Parsetree.case]
        }
      ; rewrite_value_description = {
          srctype = [%typ: value_description]
        ; dsttype = [%typ: DST.Parsetree.value_description]
        }
      ; rewrite_type_declaration = {
          srctype = [%typ: type_declaration]
        ; dsttype = [%typ: DST.Parsetree.type_declaration]
        }
      ; rewrite_type_kind = {
          srctype = [%typ: type_kind]
        ; dsttype = [%typ: DST.Parsetree.type_kind]
        }
      ; rewrite_label_declaration = {
          srctype = [%typ: label_declaration]
        ; dsttype = [%typ: DST.Parsetree.label_declaration]
        }
      ; rewrite_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        }
      ; rewrite_type_extension = {
          srctype = [%typ: type_extension]
        ; dsttype = [%typ: DST.Parsetree.type_extension]
        }
      ; rewrite_extension_constructor = {
          srctype = [%typ: extension_constructor]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor]
        }
      ; rewrite_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor_kind]
        }
      ; rewrite_class_type = {
          srctype = [%typ: class_type]
        ; dsttype = [%typ: DST.Parsetree.class_type]
        }
      ; rewrite_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        }
      ; rewrite_class_signature = {
          srctype = [%typ: class_signature]
        ; dsttype = [%typ: DST.Parsetree.class_signature]
        }
      ; rewrite_class_type_field = {
          srctype = [%typ: class_type_field]
        ; dsttype = [%typ: DST.Parsetree.class_type_field]
        }
      ; rewrite_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_field_desc]
        }
      ; rewrite_class_infos = {
          srctype = [%typ: 'a class_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.class_infos]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_class_description = {
          srctype = [%typ: class_description]
        ; dsttype = [%typ: DST.Parsetree.class_description]
        }
      ; rewrite_class_type_declaration = {
          srctype = [%typ: class_type_declaration]
        ; dsttype = [%typ: DST.Parsetree.class_type_declaration]
        }
      ; rewrite_class_expr = {
          srctype = [%typ: class_expr]
        ; dsttype = [%typ: DST.Parsetree.class_expr]
        }
      ; rewrite_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        }
      ; rewrite_class_structure = {
          srctype = [%typ: class_structure]
        ; dsttype = [%typ: DST.Parsetree.class_structure]
        }
      ; rewrite_class_field = {
          srctype = [%typ: class_field]
        ; dsttype = [%typ: DST.Parsetree.class_field]
        }
      ; rewrite_class_field_desc = {
          srctype = [%typ: class_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_field_desc]
        }
      ; rewrite_class_field_kind = {
          srctype = [%typ: class_field_kind]
        ; dsttype = [%typ: DST.Parsetree.class_field_kind]
        }
      ; rewrite_class_declaration = {
          srctype = [%typ: class_declaration]
        ; dsttype = [%typ: DST.Parsetree.class_declaration]
        }
      ; rewrite_module_type = {
          srctype = [%typ: module_type]
        ; dsttype = [%typ: DST.Parsetree.module_type]
        }
      ; rewrite_module_type_desc = {
          srctype = [%typ: module_type_desc]
        ; dsttype = [%typ: DST.Parsetree.module_type_desc]
        }
      ; rewrite_signature = {
          srctype = [%typ: signature]
        ; dsttype = [%typ: DST.Parsetree.signature]
        }
      ; rewrite_signature_item = {
          srctype = [%typ: signature_item]
        ; dsttype = [%typ: DST.Parsetree.signature_item]
        }
      ; rewrite_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        }
      ; rewrite_module_declaration = {
          srctype = [%typ: module_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_declaration]
        }
      ; rewrite_module_type_declaration = {
          srctype = [%typ: module_type_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_type_declaration]
        }
      ; rewrite_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.Parsetree.open_description]
        }
      ; rewrite_include_infos = {
          srctype = [%typ: 'a include_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.include_infos]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; rewrite_include_description = {
          srctype = [%typ: include_description]
        ; dsttype = [%typ: DST.Parsetree.include_description]
        }
      ; rewrite_include_declaration = {
          srctype = [%typ: include_declaration]
        ; dsttype = [%typ: DST.Parsetree.include_declaration]
        }
      ; rewrite_with_constraint = {
          srctype = [%typ: with_constraint]
        ; dsttype = [%typ: DST.Parsetree.with_constraint]
        }
      ; rewrite_module_expr = {
          srctype = [%typ: module_expr]
        ; dsttype = [%typ: DST.Parsetree.module_expr]
        }
      ; rewrite_module_expr_desc = {
          srctype = [%typ: module_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.module_expr_desc]
        }
      ; rewrite_structure = {
          srctype = [%typ: structure]
        ; dsttype = [%typ: DST.Parsetree.structure]
        }
      ; rewrite_structure_item = {
          srctype = [%typ: structure_item]
        ; dsttype = [%typ: DST.Parsetree.structure_item]
        }
      ; rewrite_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        }
      ; rewrite_value_binding = {
          srctype = [%typ: value_binding]
        ; dsttype = [%typ: DST.Parsetree.value_binding]
        }
      ; rewrite_module_binding = {
          srctype = [%typ: module_binding]
        ; dsttype = [%typ: DST.Parsetree.module_binding]
        }
      }
    }
]
