
module SRC = All_ast.Ast_4_06
module DST = All_ast.Ast_4_05

let src_loc_none =
  let open SRC.Lexing in
  let open SRC.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let dst_loc_none =
  let open DST.Lexing in
  let open DST.Location in
  let loc = {
    pos_fname = "";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }

let wrap_loc inh v =
  let loc = match inh with
      None -> src_loc_none
    | Some loc -> loc in
  let open SRC.Location in
  { txt = v ; loc = loc }

let map_loc f v =
  let open SRC.Location in
  { txt = f v.txt ; loc = v.loc }

let unwrap_loc v = v.SRC.Location.txt

exception Migration_error of string * SRC.Location.t option

let migration_error location feature =
  raise (Migration_error (feature, location))

let _rewrite_list subrw0 __dst__ __inh__ l =
  List.map (subrw0 __dst__ __inh__) l

type lexing_position = [%import: All_ast.Ast_4_06.Lexing.position]
and location_t = [%import: All_ast.Ast_4_06.Location.t
    [@with Lexing.position := lexing_position]
]
and 'a location_loc = [%import: 'a All_ast.Ast_4_06.Location.loc
    [@with t := location_t]
]
and longident_t = [%import: All_ast.Ast_4_06.Longident.t
    [@with t := longident_t]
]

and label = [%import: All_ast.Ast_4_06.Asttypes.label]
and arg_label = [%import: All_ast.Ast_4_06.Asttypes.arg_label]

and closed_flag =  [%import: All_ast.Ast_4_06.Asttypes.closed_flag]
and rec_flag =  [%import: All_ast.Ast_4_06.Asttypes.rec_flag]
and direction_flag =  [%import: All_ast.Ast_4_06.Asttypes.direction_flag]
and private_flag =  [%import: All_ast.Ast_4_06.Asttypes.private_flag]
and mutable_flag =  [%import: All_ast.Ast_4_06.Asttypes.mutable_flag]
and virtual_flag =  [%import: All_ast.Ast_4_06.Asttypes.virtual_flag]
and override_flag =  [%import: All_ast.Ast_4_06.Asttypes.override_flag]
and variance =  [%import: All_ast.Ast_4_06.Asttypes.variance]
and constant =  [%import: All_ast.Ast_4_06.Parsetree.constant]
and location_stack = [%import: All_ast.Ast_4_06.Parsetree.location_stack
    [@with Location.t := location_t]
]
and attribute = [%import: All_ast.Ast_4_06.Parsetree.attribute
    [@with Asttypes.loc := location_loc]
]
and extension = [%import: All_ast.Ast_4_06.Parsetree.extension
    [@with Asttypes.loc := location_loc]
]
and attributes = [%import: All_ast.Ast_4_06.Parsetree.attributes]
and payload = [%import: All_ast.Ast_4_06.Parsetree.payload]
and core_type = [%import: All_ast.Ast_4_06.Parsetree.core_type
    [@with Location.t := location_t]
]
and core_type_desc = [%import: All_ast.Ast_4_06.Parsetree.core_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.closed_flag := closed_flag
         ; Asttypes.arg_label := arg_label
         ; Asttypes.label := label
    ]
]
and package_type = [%import: All_ast.Ast_4_06.Parsetree.package_type
    [@with Longident.t := longident_t
          ; Asttypes.loc := location_loc
    ]
]
and row_field = [%import: All_ast.Ast_4_06.Parsetree.row_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and object_field = [%import: All_ast.Ast_4_06.Parsetree.object_field
    [@with
      Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and pattern = [%import: All_ast.Ast_4_06.Parsetree.pattern
    [@with Location.t := location_t]
]
and pattern_desc = [%import: All_ast.Ast_4_06.Parsetree.pattern_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.closed_flag := closed_flag
    ]
]
and expression = [%import: All_ast.Ast_4_06.Parsetree.expression
    [@with Location.t := location_t]
]
and expression_desc = [%import: All_ast.Ast_4_06.Parsetree.expression_desc
    [@with Longident.t := longident_t ;
      Asttypes.loc := location_loc ;
      Asttypes.label := label ;
      Asttypes.arg_label := arg_label ;
      Asttypes.rec_flag := rec_flag ;
      Asttypes.override_flag := override_flag ;
      Asttypes.direction_flag := direction_flag ;
    ]
]
and case = [%import: All_ast.Ast_4_06.Parsetree.case]
and value_description = [%import: All_ast.Ast_4_06.Parsetree.value_description
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and type_declaration = [%import: All_ast.Ast_4_06.Parsetree.type_declaration
    [@with Location.t := location_t
          ; Asttypes.loc := location_loc
          ; Asttypes.variance := variance
          ; Asttypes.private_flag := private_flag
    ]
]
and type_kind = [%import: All_ast.Ast_4_06.Parsetree.type_kind]
and label_declaration = [%import: All_ast.Ast_4_06.Parsetree.label_declaration
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.mutable_flag := mutable_flag
    ]
]
and constructor_declaration = [%import: All_ast.Ast_4_06.Parsetree.constructor_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and constructor_arguments = [%import: All_ast.Ast_4_06.Parsetree.constructor_arguments]
and type_extension = [%import: All_ast.Ast_4_06.Parsetree.type_extension
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.private_flag := private_flag
    ]
]
and extension_constructor = [%import: All_ast.Ast_4_06.Parsetree.extension_constructor
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and extension_constructor_kind = [%import: All_ast.Ast_4_06.Parsetree.extension_constructor_kind
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and class_type = [%import: All_ast.Ast_4_06.Parsetree.class_type
    [@with Location.t := location_t]
]
and class_type_desc = [%import: All_ast.Ast_4_06.Parsetree.class_type_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_signature = [%import: All_ast.Ast_4_06.Parsetree.class_signature]
and class_type_field = [%import: All_ast.Ast_4_06.Parsetree.class_type_field
    [@with Location.t := location_t]
]
and class_type_field_desc = [%import: All_ast.Ast_4_06.Parsetree.class_type_field_desc
    [@with
      Asttypes.private_flag := private_flag
    ; Asttypes.mutable_flag := mutable_flag
    ; Asttypes.virtual_flag := virtual_flag
    ; Asttypes.label := label
    ; Asttypes.loc := location_loc
    ]
]
and 'a class_infos = [%import: 'a All_ast.Ast_4_06.Parsetree.class_infos
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
         ; Asttypes.variance := variance
         ; Asttypes.virtual_flag := virtual_flag
    ]
]
and class_description = [%import: All_ast.Ast_4_06.Parsetree.class_description]
and class_type_declaration = [%import: All_ast.Ast_4_06.Parsetree.class_type_declaration]
and class_expr = [%import: All_ast.Ast_4_06.Parsetree.class_expr
    [@with Location.t := location_t]
]
and class_expr_desc = [%import: All_ast.Ast_4_06.Parsetree.class_expr_desc
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
         ; Asttypes.rec_flag := rec_flag
         ; Asttypes.label := label
         ; Asttypes.arg_label := arg_label
         ; Asttypes.override_flag := override_flag
    ]
]
and class_structure = [%import: All_ast.Ast_4_06.Parsetree.class_structure]
and class_field = [%import: All_ast.Ast_4_06.Parsetree.class_field
    [@with Location.t := location_t]
]
and class_field_desc = [%import: All_ast.Ast_4_06.Parsetree.class_field_desc
    [@with Asttypes.loc := location_loc
         ; Asttypes.override_flag := override_flag
         ; Asttypes.mutable_flag := mutable_flag
         ; Asttypes.private_flag := private_flag
         ; Asttypes.label := label
    ]
]
and class_field_kind = [%import: All_ast.Ast_4_06.Parsetree.class_field_kind
    [@with Asttypes.override_flag := override_flag
    ]
]
and class_declaration = [%import: All_ast.Ast_4_06.Parsetree.class_declaration]
and module_type = [%import: All_ast.Ast_4_06.Parsetree.module_type
    [@with Location.t := location_t]
]
and module_type_desc = [%import: All_ast.Ast_4_06.Parsetree.module_type_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and signature = [%import: All_ast.Ast_4_06.Parsetree.signature]
and signature_item = [%import: All_ast.Ast_4_06.Parsetree.signature_item
    [@with Location.t := location_t]
]
and signature_item_desc = [%import: All_ast.Ast_4_06.Parsetree.signature_item_desc
    [@with Asttypes.rec_flag := rec_flag]
]
and module_declaration = [%import: All_ast.Ast_4_06.Parsetree.module_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and module_type_declaration = [%import: All_ast.Ast_4_06.Parsetree.module_type_declaration
    [@with Location.t := location_t ;
           Asttypes.loc := location_loc
    ]
]
and open_description = [%import: All_ast.Ast_4_06.Parsetree.open_description
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.override_flag := override_flag
    ]
]
and 'a include_infos = [%import: 'a All_ast.Ast_4_06.Parsetree.include_infos
    [@with Location.t := location_t]
]
and include_description = [%import: All_ast.Ast_4_06.Parsetree.include_description]
and include_declaration = [%import: All_ast.Ast_4_06.Parsetree.include_declaration]
and with_constraint = [%import: All_ast.Ast_4_06.Parsetree.with_constraint
    [@with Longident.t := longident_t
         ; Asttypes.loc := location_loc
    ]
]
and module_expr = [%import: All_ast.Ast_4_06.Parsetree.module_expr
    [@with Location.t := location_t]
]
and module_expr_desc = [%import: All_ast.Ast_4_06.Parsetree.module_expr_desc
    [@with Longident.t := longident_t ;
           Asttypes.loc := location_loc
    ]
]
and structure = [%import: All_ast.Ast_4_06.Parsetree.structure]
and structure_item = [%import: All_ast.Ast_4_06.Parsetree.structure_item
    [@with Location.t := location_t]
]
and structure_item_desc = [%import: All_ast.Ast_4_06.Parsetree.structure_item_desc
    [@with Location.t := location_t
          ; Longident.t := longident_t
          ; Asttypes.loc := location_loc
          ; Asttypes.rec_flag := rec_flag
    ]
]
and value_binding = [%import: All_ast.Ast_4_06.Parsetree.value_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and module_binding = [%import: All_ast.Ast_4_06.Parsetree.module_binding
    [@with Location.t := location_t
         ; Asttypes.loc := location_loc
    ]
]
and out_ident = [%import: All_ast.Ast_4_06.Outcometree.out_ident]
and out_string = [%import: All_ast.Ast_4_06.Outcometree.out_string]
and out_attribute = [%import: All_ast.Ast_4_06.Outcometree.out_attribute]
and out_value = [%import: All_ast.Ast_4_06.Outcometree.out_value]
and out_type = [%import: All_ast.Ast_4_06.Outcometree.out_type]
and out_variant = [%import: All_ast.Ast_4_06.Outcometree.out_variant]
and out_class_type = [%import: All_ast.Ast_4_06.Outcometree.out_class_type]
and out_class_sig_item = [%import: All_ast.Ast_4_06.Outcometree.out_class_sig_item]
and out_module_type = [%import: All_ast.Ast_4_06.Outcometree.out_module_type]
and out_sig_item = [%import: All_ast.Ast_4_06.Outcometree.out_sig_item]
and out_type_decl = [%import: All_ast.Ast_4_06.Outcometree.out_type_decl
    [@with Asttypes.private_flag := private_flag]
]
and out_extension_constructor = [%import: All_ast.Ast_4_06.Outcometree.out_extension_constructor
    [@with Asttypes.private_flag := private_flag]
]
and out_type_extension = [%import: All_ast.Ast_4_06.Outcometree.out_type_extension
    [@with Asttypes.private_flag := private_flag]
]
and out_val_decl = [%import: All_ast.Ast_4_06.Outcometree.out_val_decl]
and out_rec_status = [%import: All_ast.Ast_4_06.Outcometree.out_rec_status]
and out_ext_status = [%import: All_ast.Ast_4_06.Outcometree.out_ext_status]
and out_phrase = [%import: All_ast.Ast_4_06.Outcometree.out_phrase]


[@@deriving rewrite
    { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_value = dt
    ; dispatchers = {
        rewrite_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; rewrite_Lexing_position = {
          srctype = [%typ: lexing_position]
        ; dsttype = [%typ: DST.Lexing.position]
        }
      ; rewrite_Location_t = {
          srctype = [%typ: location_t]
        ; dsttype = [%typ: DST.Location.t]
        }
      ; rewrite_string_Location_loc = {
          srctype = [%typ: string location_loc]
        ; dsttype = [%typ: string DST.Location.loc]
        }
      ; rewrite_label_Location_loc = {
          srctype = [%typ: label location_loc]
        ; dsttype = [%typ: label DST.Location.loc]
        }
      ; rewrite_longident_Location_loc = {
          srctype = [%typ: longident_t location_loc]
        ; dsttype = [%typ: DST.Longident.t DST.Location.loc]
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
      ; rewrite_arg_label = {
          srctype = [%typ: arg_label]
        ; dsttype = [%typ: DST.Asttypes.arg_label]
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
        ; inherit_code = Some ptyp_loc
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
        ; custom_branches_code = function
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Rtag
                (__dt__.rewrite_label __dt__ __inh__ (unwrap_loc v_0),
                 __dt__.rewrite_attributes __dt__ __inh__ v_1,
                 v_2,
                 List.map (__dt__.rewrite_core_type __dt__ __inh__) v_3)
        }
      ; rewrite_object_field = {
          srctype = [%typ: object_field]
        ; dsttype = [%typ: (string DST.Asttypes.loc * DST.Parsetree.attributes * DST.Parsetree.core_type)]
        ; dstmodule = DST.Parsetree
        ; code = fun __dst__ __inh__ -> function
            Otag (ll, al, ct) ->
              let open DST.Parsetree in
              (__dst__.rewrite_label_Location_loc __dst__ __inh__ ll,
               __dst__.rewrite_attributes __dst__ __inh__ al,
               __dst__.rewrite_core_type __dst__ __inh__ ct)
            | Oinherit _ -> migration_error __inh__ "Oinherit"
        }
      ; rewrite_pattern = {
          srctype = [%typ: pattern]
        ; dsttype = [%typ: DST.Parsetree.pattern]
        ; inherit_code = Some ppat_loc
        }
      ; rewrite_pattern_desc = {
          srctype = [%typ: pattern_desc]
        ; dsttype = [%typ: DST.Parsetree.pattern_desc]
        }
      ; rewrite_expression = {
          srctype = [%typ: expression]
        ; dsttype = [%typ: DST.Parsetree.expression]
        ; inherit_code = Some pexp_loc
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
        ; inherit_code = Some pval_loc
        }
      ; rewrite_type_declaration = {
          srctype = [%typ: type_declaration]
        ; dsttype = [%typ: DST.Parsetree.type_declaration]
        ; inherit_code = Some ptype_loc
        }
      ; rewrite_type_kind = {
          srctype = [%typ: type_kind]
        ; dsttype = [%typ: DST.Parsetree.type_kind]
        }
      ; rewrite_label_declaration = {
          srctype = [%typ: label_declaration]
        ; dsttype = [%typ: DST.Parsetree.label_declaration]
        ; inherit_code = Some pld_loc
        }
      ; rewrite_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        ; inherit_code = Some pcd_loc
        }
      ; rewrite_constructor_arguments = {
          srctype = [%typ: constructor_arguments]
        ; dsttype = [%typ: DST.Parsetree.constructor_arguments]
        }
      ; rewrite_type_extension = {
          srctype = [%typ: type_extension]
        ; dsttype = [%typ: DST.Parsetree.type_extension]
        }
      ; rewrite_extension_constructor = {
          srctype = [%typ: extension_constructor]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor]
        ; inherit_code = Some pext_loc
        }
      ; rewrite_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor_kind]
        }
      ; rewrite_class_type = {
          srctype = [%typ: class_type]
        ; dsttype = [%typ: DST.Parsetree.class_type]
        ; inherit_code = Some pcty_loc
        }
      ; rewrite_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        ; custom_branches_code = function
            | Pcty_open _ ->migration_error __inh__ "Pcty_open"
        }
      ; rewrite_class_signature = {
          srctype = [%typ: class_signature]
        ; dsttype = [%typ: DST.Parsetree.class_signature]
        }
      ; rewrite_class_type_field = {
          srctype = [%typ: class_type_field]
        ; dsttype = [%typ: DST.Parsetree.class_type_field]
        ; inherit_code = Some pctf_loc
        }
      ; rewrite_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_field_desc]
        }
      ; rewrite_class_infos = {
          srctype = [%typ: 'a class_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.class_infos]
        ; inherit_code = Some pci_loc
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
        ; inherit_code = Some pcl_loc
        }
      ; rewrite_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        ; custom_branches_code = function
            | Pcl_open _ -> migration_error __inh__ "Pcl_open"
        }
      ; rewrite_class_structure = {
          srctype = [%typ: class_structure]
        ; dsttype = [%typ: DST.Parsetree.class_structure]
        }
      ; rewrite_class_field = {
          srctype = [%typ: class_field]
        ; dsttype = [%typ: DST.Parsetree.class_field]
        ; inherit_code = Some pcf_loc
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
        ; inherit_code = Some pmty_loc
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
        ; inherit_code = Some psig_loc
        }
      ; rewrite_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        }
      ; rewrite_module_declaration = {
          srctype = [%typ: module_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_declaration]
        ; inherit_code = Some pmd_loc
        }
      ; rewrite_module_type_declaration = {
          srctype = [%typ: module_type_declaration]
        ; dsttype = [%typ: DST.Parsetree.module_type_declaration]
        ; inherit_code = Some pmtd_loc
        }
      ; rewrite_open_description = {
          srctype = [%typ: open_description]
        ; dsttype = [%typ: DST.Parsetree.open_description]
        ; inherit_code = Some popen_loc
        }
      ; rewrite_include_infos = {
          srctype = [%typ: 'a include_infos]
        ; dsttype = [%typ: 'b DST.Parsetree.include_infos]
        ; inherit_code = Some pincl_loc
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
        ; custom_branches_code = function
            | Pwith_typesubst ({txt=Lident _;}, v_1) ->
              let open DST.Parsetree in
              Pwith_typesubst (__dt__.rewrite_type_declaration __dt__ __inh__ v_1)
            | Pwith_typesubst _ -> migration_error __inh__ "Pwith_typesubst:longident"
            | Pwith_modsubst (v_0, v_1) ->
              let v_0 = map_loc (function
                    Lident s -> s
                  | _ -> migration_error __inh__ "Pwith_modsubst:longident") v_0 in
              let open DST.Parsetree in
              Pwith_modsubst
                (__dt__.rewrite_string_Location_loc __dt__ __inh__ v_0,
                 __dt__.rewrite_longident_Location_loc __dt__ __inh__ v_1)
        }
      ; rewrite_module_expr = {
          srctype = [%typ: module_expr]
        ; dsttype = [%typ: DST.Parsetree.module_expr]
        ; inherit_code = Some pmod_loc
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
        ; inherit_code = Some pstr_loc
        }
      ; rewrite_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        }
      ; rewrite_value_binding = {
          srctype = [%typ: value_binding]
        ; dsttype = [%typ: DST.Parsetree.value_binding]
        ; inherit_code = Some pvb_loc
        }
      ; rewrite_module_binding = {
          srctype = [%typ: module_binding]
        ; dsttype = [%typ: DST.Parsetree.module_binding]
        ; inherit_code = Some pmb_loc
        }
      ; rewrite_out_ident = {
          srctype = [%typ: out_ident]
        ; dsttype = [%typ: DST.Outcometree.out_ident]
        }
      ; rewrite_out_attribute = {
          srctype = [%typ: out_attribute]
        ; dsttype = [%typ: DST.Outcometree.out_attribute]
        }
      ; rewrite_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; rewrite_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; rewrite_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.Outcometree.out_value]
        ; custom_branches_code = function
            | Oval_string (s, _, _) -> 
              let open DST.Parsetree in
              Oval_string s
        }
      ; rewrite_out_type = {
          srctype = [%typ: out_type]
        ; dsttype = [%typ: DST.Outcometree.out_type]
        }
      ; rewrite_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.Outcometree.out_variant]
        }
      ; rewrite_out_class_type = {
          srctype = [%typ: out_class_type]
        ; dsttype = [%typ: DST.Outcometree.out_class_type]
        }
      ; rewrite_out_class_sig_item = {
          srctype = [%typ: out_class_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_class_sig_item]
        }
      ; rewrite_out_module_type = {
          srctype = [%typ: out_module_type]
        ; dsttype = [%typ: DST.Outcometree.out_module_type]
        }
      ; rewrite_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        }
      ; rewrite_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        }
      ; rewrite_out_extension_constructor = {
          srctype = [%typ: out_extension_constructor]
        ; dsttype = [%typ: DST.Outcometree.out_extension_constructor]
        }
      ; rewrite_out_type_extension = {
          srctype = [%typ: out_type_extension]
        ; dsttype = [%typ: DST.Outcometree.out_type_extension]
        }
      ; rewrite_out_val_decl = {
          srctype = [%typ: out_val_decl]
        ; dsttype = [%typ: DST.Outcometree.out_val_decl]
        }
      ; rewrite_out_rec_status = {
          srctype = [%typ: out_rec_status]
        ; dsttype = [%typ: DST.Outcometree.out_rec_status]
        }
      ; rewrite_out_ext_status = {
          srctype = [%typ: out_ext_status]
        ; dsttype = [%typ: DST.Outcometree.out_ext_status]
        }
      ; rewrite_out_phrase = {
          srctype = [%typ: out_phrase]
        ; dsttype = [%typ: DST.Outcometree.out_phrase]
        }
      }
    }
]
