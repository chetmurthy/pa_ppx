
# 2 "all_ast.ORIG.ml"
module Ast_4_02 = struct
# 1 "gen/ast_4_02.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Location =
  struct
    type t =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
    type constant =
      [ Const_int of int
      | Const_char of char
      | Const_string of string and option string
      | Const_float of string
      | Const_int32 of int32
      | Const_int64 of int64
      | Const_nativeint of nativeint ]
    ;
  end
;
module Parsetree =
  struct
    open Asttypes;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list (string * attributes * core_type) and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list string and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of label and option expression and pattern and expression
      | Pexp_apply of expression and list (label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and string
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc string and expression
      | Pexp_override of list (loc string * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : list core_type;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of list core_type and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of label and core_type and class_type
      | Pcty_extension of extension ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (string * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option string
      | Pcf_val of (loc string * mutable_flag * class_field_kind)
      | Pcf_method of (loc string * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of type_declaration
      | Pwith_modsubst of loc string and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_name of out_ident and list out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of string and out_type and list string ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 4 "all_ast.ORIG.ml"
end;

module Ast_4_03 = struct
# 1 "gen/ast_4_03.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Location =
  struct
    type t =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list (string * attributes * core_type) and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list string and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and string
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc string and expression
      | Pexp_override of list (loc string * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (string * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option string
      | Pcf_val of (loc string * mutable_flag * class_field_kind)
      | Pcf_method of (loc string * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of type_declaration
      | Pwith_modsubst of loc string and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_name of out_ident and list out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 8 "all_ast.ORIG.ml"
end;

module Ast_4_04 = struct
# 1 "gen/ast_4_04.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Location =
  struct
    type t =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list (string * attributes * core_type) and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list string and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and string
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc string and expression
      | Pexp_override of list (loc string * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (string * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option string
      | Pcf_val of (loc string * mutable_flag * class_field_kind)
      | Pcf_method of (loc string * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of type_declaration
      | Pwith_modsubst of loc string and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_name of out_ident and list out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 12 "all_ast.ORIG.ml"
end;

module Ast_4_05 = struct
# 1 "gen/ast_4_05.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Location =
  struct
    type t =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of
          list (loc string * attributes * core_type) and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc string
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc string and expression
      | Pexp_override of list (loc string * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc string * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc string * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc string * mutable_flag * class_field_kind)
      | Pcf_method of (loc string * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of type_declaration
      | Pwith_modsubst of loc string and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 16 "all_ast.ORIG.ml"
end;

module Ast_4_06 = struct
# 1 "gen/ast_4_06.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of loc label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      [ Otag of loc label and attributes and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of override_flag and loc Longident.t and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of override_flag and loc Longident.t and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 20 "all_ast.ORIG.ml"
end;

module Ast_4_07 = struct
# 1 "gen/ast_4_07.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute = (loc string * payload)
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      [ Rtag of loc label and attributes and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      [ Otag of loc label and attributes and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of override_flag and loc Longident.t and expression
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of override_flag and loc Longident.t and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of override_flag and loc Longident.t and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of extension_constructor
      | Psig_module of module_declaration
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_description =
      { popen_lid : loc Longident.t;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of extension_constructor
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_description
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of string ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of string and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 24 "all_ast.ORIG.ml"
end;

module Ast_4_08 = struct
# 1 "gen/ast_4_08.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute =
      { attr_name : loc string;
        attr_payload : payload;
        attr_loc : Location.t }
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_loc_stack : list Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      { prf_desc : row_field_desc;
        prf_loc : Location.t;
        prf_attributes : attributes }
    and row_field_desc =
      [ Rtag of loc label and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      { pof_desc : object_field_desc;
        pof_loc : Location.t;
        pof_attributes : attributes }
    and object_field_desc =
      [ Otag of loc label and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_loc_stack : list Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_loc_stack : list Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of open_declaration and expression
      | Pexp_letop of letop
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and letop =
      { let_ : binding_op; ands : list binding_op; body : expression }
    and binding_op =
      { pbop_op : loc string;
        pbop_pat : pattern;
        pbop_exp : expression;
        pbop_loc : Location.t }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_loc : Location.t;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and type_exception =
      { ptyexn_constructor : extension_constructor;
        ptyexn_loc : Location.t;
        ptyexn_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of open_description and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of open_description and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typesubst of list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of type_exception
      | Psig_module of module_declaration
      | Psig_modsubst of module_substitution
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_substitution =
      { pms_name : loc string;
        pms_manifest : loc Longident.t;
        pms_attributes : attributes;
        pms_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_infos α =
      { popen_expr : α;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and open_description = open_infos (loc Longident.t)
    and open_declaration = open_infos module_expr
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of type_exception
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_declaration
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_name = { printed_name : mutable string };
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of out_name ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of out_ident and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 28 "all_ast.ORIG.ml"
end;

module Ast_4_09 = struct
# 1 "gen/ast_4_09.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute =
      { attr_name : loc string;
        attr_payload : payload;
        attr_loc : Location.t }
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_loc_stack : list Location.t;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      { prf_desc : row_field_desc;
        prf_loc : Location.t;
        prf_attributes : attributes }
    and row_field_desc =
      [ Rtag of loc label and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      { pof_desc : object_field_desc;
        pof_loc : Location.t;
        pof_attributes : attributes }
    and object_field_desc =
      [ Otag of loc label and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_loc_stack : list Location.t;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc string
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_loc_stack : list Location.t;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc string and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of open_declaration and expression
      | Pexp_letop of letop
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and letop =
      { let_ : binding_op; ands : list binding_op; body : expression }
    and binding_op =
      { pbop_op : loc string;
        pbop_pat : pattern;
        pbop_exp : expression;
        pbop_loc : Location.t }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_loc : Location.t;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and type_exception =
      { ptyexn_constructor : extension_constructor;
        ptyexn_loc : Location.t;
        ptyexn_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of open_description and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of open_description and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of loc string and option module_type and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typesubst of list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of type_exception
      | Psig_module of module_declaration
      | Psig_modsubst of module_substitution
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc string;
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_substitution =
      { pms_name : loc string;
        pms_manifest : loc Longident.t;
        pms_attributes : attributes;
        pms_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_infos α =
      { popen_expr : α;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and open_description = open_infos (loc Longident.t)
    and open_declaration = open_infos module_expr
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of loc string and option module_type and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of type_exception
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_declaration
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc string;
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Outcometree =
  struct
    type out_name = { printed_name : mutable string };
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of out_name ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of out_ident and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of string and option out_module_type and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : bool;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 32 "all_ast.ORIG.ml"
end;

module Ast_4_10 = struct
# 1 "gen/ast_4_10.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute =
      { attr_name : loc string;
        attr_payload : payload;
        attr_loc : Location.t }
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_loc_stack : location_stack;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      { prf_desc : row_field_desc;
        prf_loc : Location.t;
        prf_attributes : attributes }
    and row_field_desc =
      [ Rtag of loc label and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      { pof_desc : object_field_desc;
        pof_loc : Location.t;
        pof_attributes : attributes }
    and object_field_desc =
      [ Otag of loc label and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_loc_stack : location_stack;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc (option string)
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_loc_stack : location_stack;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc (option string) and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of open_declaration and expression
      | Pexp_letop of letop
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and letop =
      { let_ : binding_op; ands : list binding_op; body : expression }
    and binding_op =
      { pbop_op : loc string;
        pbop_pat : pattern;
        pbop_exp : expression;
        pbop_loc : Location.t }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_loc : Location.t;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and type_exception =
      { ptyexn_constructor : extension_constructor;
        ptyexn_loc : Location.t;
        ptyexn_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of open_description and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of open_description and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of functor_parameter and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and functor_parameter =
      [ Unit
      | Named of loc (option string) and module_type ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typesubst of list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of type_exception
      | Psig_module of module_declaration
      | Psig_modsubst of module_substitution
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc (option string);
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_substitution =
      { pms_name : loc string;
        pms_manifest : loc Longident.t;
        pms_attributes : attributes;
        pms_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_infos α =
      { popen_expr : α;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and open_description = open_infos (loc Longident.t)
    and open_declaration = open_infos module_expr
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of functor_parameter and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of type_exception
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_declaration
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc (option string);
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Type_immediacy =
  struct
    type t = [ Unknown | Always | Always_on_64bits ];
  end
;
module Outcometree =
  struct
    type out_name = { printed_name : mutable string };
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of out_name ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of out_ident and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of
          option (option string * out_module_type) and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : Type_immediacy.t;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 36 "all_ast.ORIG.ml"
end;

module Ast_4_11 = struct
# 1 "gen/ast_4_11.ml"
module Lexing =
  struct
    type position =
      { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
    ;
  end
;
module Warnings =
  struct
    type loc =
      { loc_start : Lexing.position;
        loc_end : Lexing.position;
        loc_ghost : bool }
    ;
  end
;
module Location =
  struct
    type t =
      Warnings.loc ==
        { loc_start : Lexing.position;
          loc_end : Lexing.position;
          loc_ghost : bool }
    ;
    type loc α = { txt : α; loc : t };
  end
;
module Longident =
  struct
    type t =
      [ Lident of string
      | Ldot of t and string
      | Lapply of t and t ]
    ;
  end
;
module Asttypes =
  struct
    type loc α = Location.loc α == { txt : α; loc : Location.t };
    type arg_label =
      [ Nolabel
      | Labelled of string
      | Optional of string ]
    ;
    type label = string;
    type closed_flag = [ Closed | Open ];
    type rec_flag = [ Nonrecursive | Recursive ];
    type direction_flag = [ Upto | Downto ];
    type private_flag = [ Private | Public ];
    type mutable_flag = [ Immutable | Mutable ];
    type virtual_flag = [ Virtual | Concrete ];
    type override_flag = [ Override | Fresh ];
    type variance = [ Covariant | Contravariant | Invariant ];
  end
;
module Parsetree =
  struct
    open Asttypes;
    type constant =
      [ Pconst_integer of string and option char
      | Pconst_char of char
      | Pconst_string of string and Location.t and option string
      | Pconst_float of string and option char ]
    ;
    type location_stack = list Location.t;
    type attribute =
      { attr_name : loc string;
        attr_payload : payload;
        attr_loc : Location.t }
    and extension = (loc string * payload)
    and attributes = list attribute
    and payload =
      [ PStr of structure
      | PSig of signature
      | PTyp of core_type
      | PPat of pattern and option expression ]
    and core_type =
      { ptyp_desc : core_type_desc;
        ptyp_loc : Location.t;
        ptyp_loc_stack : location_stack;
        ptyp_attributes : attributes }
    and core_type_desc =
      [ Ptyp_any
      | Ptyp_var of string
      | Ptyp_arrow of arg_label and core_type and core_type
      | Ptyp_tuple of list core_type
      | Ptyp_constr of loc Longident.t and list core_type
      | Ptyp_object of list object_field and closed_flag
      | Ptyp_class of loc Longident.t and list core_type
      | Ptyp_alias of core_type and string
      | Ptyp_variant of list row_field and closed_flag and option (list label)
      | Ptyp_poly of list (loc string) and core_type
      | Ptyp_package of package_type
      | Ptyp_extension of extension ]
    and package_type = (loc Longident.t * list (loc Longident.t * core_type))
    and row_field =
      { prf_desc : row_field_desc;
        prf_loc : Location.t;
        prf_attributes : attributes }
    and row_field_desc =
      [ Rtag of loc label and bool and list core_type
      | Rinherit of core_type ]
    and object_field =
      { pof_desc : object_field_desc;
        pof_loc : Location.t;
        pof_attributes : attributes }
    and object_field_desc =
      [ Otag of loc label and core_type
      | Oinherit of core_type ]
    and pattern =
      { ppat_desc : pattern_desc;
        ppat_loc : Location.t;
        ppat_loc_stack : location_stack;
        ppat_attributes : attributes }
    and pattern_desc =
      [ Ppat_any
      | Ppat_var of loc string
      | Ppat_alias of pattern and loc string
      | Ppat_constant of constant
      | Ppat_interval of constant and constant
      | Ppat_tuple of list pattern
      | Ppat_construct of loc Longident.t and option pattern
      | Ppat_variant of label and option pattern
      | Ppat_record of list (loc Longident.t * pattern) and closed_flag
      | Ppat_array of list pattern
      | Ppat_or of pattern and pattern
      | Ppat_constraint of pattern and core_type
      | Ppat_type of loc Longident.t
      | Ppat_lazy of pattern
      | Ppat_unpack of loc (option string)
      | Ppat_exception of pattern
      | Ppat_extension of extension
      | Ppat_open of loc Longident.t and pattern ]
    and expression =
      { pexp_desc : expression_desc;
        pexp_loc : Location.t;
        pexp_loc_stack : location_stack;
        pexp_attributes : attributes }
    and expression_desc =
      [ Pexp_ident of loc Longident.t
      | Pexp_constant of constant
      | Pexp_let of rec_flag and list value_binding and expression
      | Pexp_function of list case
      | Pexp_fun of arg_label and option expression and pattern and expression
      | Pexp_apply of expression and list (arg_label * expression)
      | Pexp_match of expression and list case
      | Pexp_try of expression and list case
      | Pexp_tuple of list expression
      | Pexp_construct of loc Longident.t and option expression
      | Pexp_variant of label and option expression
      | Pexp_record of
          list (loc Longident.t * expression) and option expression
      | Pexp_field of expression and loc Longident.t
      | Pexp_setfield of expression and loc Longident.t and expression
      | Pexp_array of list expression
      | Pexp_ifthenelse of expression and expression and option expression
      | Pexp_sequence of expression and expression
      | Pexp_while of expression and expression
      | Pexp_for of
          pattern and expression and expression and direction_flag and
            expression
      | Pexp_constraint of expression and core_type
      | Pexp_coerce of expression and option core_type and core_type
      | Pexp_send of expression and loc label
      | Pexp_new of loc Longident.t
      | Pexp_setinstvar of loc label and expression
      | Pexp_override of list (loc label * expression)
      | Pexp_letmodule of loc (option string) and module_expr and expression
      | Pexp_letexception of extension_constructor and expression
      | Pexp_assert of expression
      | Pexp_lazy of expression
      | Pexp_poly of expression and option core_type
      | Pexp_object of class_structure
      | Pexp_newtype of loc string and expression
      | Pexp_pack of module_expr
      | Pexp_open of open_declaration and expression
      | Pexp_letop of letop
      | Pexp_extension of extension
      | Pexp_unreachable ]
    and case =
      { pc_lhs : pattern; pc_guard : option expression; pc_rhs : expression }
    and letop =
      { let_ : binding_op; ands : list binding_op; body : expression }
    and binding_op =
      { pbop_op : loc string;
        pbop_pat : pattern;
        pbop_exp : expression;
        pbop_loc : Location.t }
    and value_description =
      { pval_name : loc string;
        pval_type : core_type;
        pval_prim : list string;
        pval_attributes : attributes;
        pval_loc : Location.t }
    and type_declaration =
      { ptype_name : loc string;
        ptype_params : list (core_type * variance);
        ptype_cstrs : list (core_type * core_type * Location.t);
        ptype_kind : type_kind;
        ptype_private : private_flag;
        ptype_manifest : option core_type;
        ptype_attributes : attributes;
        ptype_loc : Location.t }
    and type_kind =
      [ Ptype_abstract
      | Ptype_variant of list constructor_declaration
      | Ptype_record of list label_declaration
      | Ptype_open ]
    and label_declaration =
      { pld_name : loc string;
        pld_mutable : mutable_flag;
        pld_type : core_type;
        pld_loc : Location.t;
        pld_attributes : attributes }
    and constructor_declaration =
      { pcd_name : loc string;
        pcd_args : constructor_arguments;
        pcd_res : option core_type;
        pcd_loc : Location.t;
        pcd_attributes : attributes }
    and constructor_arguments =
      [ Pcstr_tuple of list core_type
      | Pcstr_record of list label_declaration ]
    and type_extension =
      { ptyext_path : loc Longident.t;
        ptyext_params : list (core_type * variance);
        ptyext_constructors : list extension_constructor;
        ptyext_private : private_flag;
        ptyext_loc : Location.t;
        ptyext_attributes : attributes }
    and extension_constructor =
      { pext_name : loc string;
        pext_kind : extension_constructor_kind;
        pext_loc : Location.t;
        pext_attributes : attributes }
    and type_exception =
      { ptyexn_constructor : extension_constructor;
        ptyexn_loc : Location.t;
        ptyexn_attributes : attributes }
    and extension_constructor_kind =
      [ Pext_decl of constructor_arguments and option core_type
      | Pext_rebind of loc Longident.t ]
    and class_type =
      { pcty_desc : class_type_desc;
        pcty_loc : Location.t;
        pcty_attributes : attributes }
    and class_type_desc =
      [ Pcty_constr of loc Longident.t and list core_type
      | Pcty_signature of class_signature
      | Pcty_arrow of arg_label and core_type and class_type
      | Pcty_extension of extension
      | Pcty_open of open_description and class_type ]
    and class_signature =
      { pcsig_self : core_type; pcsig_fields : list class_type_field }
    and class_type_field =
      { pctf_desc : class_type_field_desc;
        pctf_loc : Location.t;
        pctf_attributes : attributes }
    and class_type_field_desc =
      [ Pctf_inherit of class_type
      | Pctf_val of (loc label * mutable_flag * virtual_flag * core_type)
      | Pctf_method of (loc label * private_flag * virtual_flag * core_type)
      | Pctf_constraint of (core_type * core_type)
      | Pctf_attribute of attribute
      | Pctf_extension of extension ]
    and class_infos α =
      { pci_virt : virtual_flag;
        pci_params : list (core_type * variance);
        pci_name : loc string;
        pci_expr : α;
        pci_loc : Location.t;
        pci_attributes : attributes }
    and class_description = class_infos class_type
    and class_type_declaration = class_infos class_type
    and class_expr =
      { pcl_desc : class_expr_desc;
        pcl_loc : Location.t;
        pcl_attributes : attributes }
    and class_expr_desc =
      [ Pcl_constr of loc Longident.t and list core_type
      | Pcl_structure of class_structure
      | Pcl_fun of arg_label and option expression and pattern and class_expr
      | Pcl_apply of class_expr and list (arg_label * expression)
      | Pcl_let of rec_flag and list value_binding and class_expr
      | Pcl_constraint of class_expr and class_type
      | Pcl_extension of extension
      | Pcl_open of open_description and class_expr ]
    and class_structure =
      { pcstr_self : pattern; pcstr_fields : list class_field }
    and class_field =
      { pcf_desc : class_field_desc;
        pcf_loc : Location.t;
        pcf_attributes : attributes }
    and class_field_desc =
      [ Pcf_inherit of override_flag and class_expr and option (loc string)
      | Pcf_val of (loc label * mutable_flag * class_field_kind)
      | Pcf_method of (loc label * private_flag * class_field_kind)
      | Pcf_constraint of (core_type * core_type)
      | Pcf_initializer of expression
      | Pcf_attribute of attribute
      | Pcf_extension of extension ]
    and class_field_kind =
      [ Cfk_virtual of core_type
      | Cfk_concrete of override_flag and expression ]
    and class_declaration = class_infos class_expr
    and module_type =
      { pmty_desc : module_type_desc;
        pmty_loc : Location.t;
        pmty_attributes : attributes }
    and module_type_desc =
      [ Pmty_ident of loc Longident.t
      | Pmty_signature of signature
      | Pmty_functor of functor_parameter and module_type
      | Pmty_with of module_type and list with_constraint
      | Pmty_typeof of module_expr
      | Pmty_extension of extension
      | Pmty_alias of loc Longident.t ]
    and functor_parameter =
      [ Unit
      | Named of loc (option string) and module_type ]
    and signature = list signature_item
    and signature_item =
      { psig_desc : signature_item_desc; psig_loc : Location.t }
    and signature_item_desc =
      [ Psig_value of value_description
      | Psig_type of rec_flag and list type_declaration
      | Psig_typesubst of list type_declaration
      | Psig_typext of type_extension
      | Psig_exception of type_exception
      | Psig_module of module_declaration
      | Psig_modsubst of module_substitution
      | Psig_recmodule of list module_declaration
      | Psig_modtype of module_type_declaration
      | Psig_open of open_description
      | Psig_include of include_description
      | Psig_class of list class_description
      | Psig_class_type of list class_type_declaration
      | Psig_attribute of attribute
      | Psig_extension of extension and attributes ]
    and module_declaration =
      { pmd_name : loc (option string);
        pmd_type : module_type;
        pmd_attributes : attributes;
        pmd_loc : Location.t }
    and module_substitution =
      { pms_name : loc string;
        pms_manifest : loc Longident.t;
        pms_attributes : attributes;
        pms_loc : Location.t }
    and module_type_declaration =
      { pmtd_name : loc string;
        pmtd_type : option module_type;
        pmtd_attributes : attributes;
        pmtd_loc : Location.t }
    and open_infos α =
      { popen_expr : α;
        popen_override : override_flag;
        popen_loc : Location.t;
        popen_attributes : attributes }
    and open_description = open_infos (loc Longident.t)
    and open_declaration = open_infos module_expr
    and include_infos α =
      { pincl_mod : α; pincl_loc : Location.t; pincl_attributes : attributes }
    and include_description = include_infos module_type
    and include_declaration = include_infos module_expr
    and with_constraint =
      [ Pwith_type of loc Longident.t and type_declaration
      | Pwith_module of loc Longident.t and loc Longident.t
      | Pwith_typesubst of loc Longident.t and type_declaration
      | Pwith_modsubst of loc Longident.t and loc Longident.t ]
    and module_expr =
      { pmod_desc : module_expr_desc;
        pmod_loc : Location.t;
        pmod_attributes : attributes }
    and module_expr_desc =
      [ Pmod_ident of loc Longident.t
      | Pmod_structure of structure
      | Pmod_functor of functor_parameter and module_expr
      | Pmod_apply of module_expr and module_expr
      | Pmod_constraint of module_expr and module_type
      | Pmod_unpack of expression
      | Pmod_extension of extension ]
    and structure = list structure_item
    and structure_item =
      { pstr_desc : structure_item_desc; pstr_loc : Location.t }
    and structure_item_desc =
      [ Pstr_eval of expression and attributes
      | Pstr_value of rec_flag and list value_binding
      | Pstr_primitive of value_description
      | Pstr_type of rec_flag and list type_declaration
      | Pstr_typext of type_extension
      | Pstr_exception of type_exception
      | Pstr_module of module_binding
      | Pstr_recmodule of list module_binding
      | Pstr_modtype of module_type_declaration
      | Pstr_open of open_declaration
      | Pstr_class of list class_declaration
      | Pstr_class_type of list class_type_declaration
      | Pstr_include of include_declaration
      | Pstr_attribute of attribute
      | Pstr_extension of extension and attributes ]
    and value_binding =
      { pvb_pat : pattern;
        pvb_expr : expression;
        pvb_attributes : attributes;
        pvb_loc : Location.t }
    and module_binding =
      { pmb_name : loc (option string);
        pmb_expr : module_expr;
        pmb_attributes : attributes;
        pmb_loc : Location.t }
    ;
  end
;
module Type_immediacy =
  struct
    type t = [ Unknown | Always | Always_on_64bits ];
  end
;
module Outcometree =
  struct
    type out_name = { printed_name : mutable string };
    type out_ident =
      [ Oide_apply of out_ident and out_ident
      | Oide_dot of out_ident and string
      | Oide_ident of out_name ]
    ;
    type out_string = [ Ostr_string | Ostr_bytes ];
    type out_attribute = { oattr_name : string };
    type out_value =
      [ Oval_array of list out_value
      | Oval_char of char
      | Oval_constr of out_ident and list out_value
      | Oval_ellipsis
      | Oval_float of float
      | Oval_int of int
      | Oval_int32 of int32
      | Oval_int64 of int64
      | Oval_nativeint of nativeint
      | Oval_list of list out_value
      | Oval_printer of Format.formatter → unit
      | Oval_record of list (out_ident * out_value)
      | Oval_string of string and int and out_string
      | Oval_stuff of string
      | Oval_tuple of list out_value
      | Oval_variant of string and option out_value ]
    ;
    type out_type =
      [ Otyp_abstract
      | Otyp_open
      | Otyp_alias of out_type and string
      | Otyp_arrow of string and out_type and out_type
      | Otyp_class of bool and out_ident and list out_type
      | Otyp_constr of out_ident and list out_type
      | Otyp_manifest of out_type and out_type
      | Otyp_object of list (string * out_type) and option bool
      | Otyp_record of list (string * bool * out_type)
      | Otyp_stuff of string
      | Otyp_sum of list (string * list out_type * option out_type)
      | Otyp_tuple of list out_type
      | Otyp_var of bool and string
      | Otyp_variant of bool and out_variant and bool and option (list string)
      | Otyp_poly of list string and out_type
      | Otyp_module of out_ident and list string and list out_type
      | Otyp_attribute of out_type and out_attribute ]
    and out_variant =
      [ Ovar_fields of list (string * bool * list out_type)
      | Ovar_typ of out_type ]
    ;
    type out_class_type =
      [ Octy_constr of out_ident and list out_type
      | Octy_arrow of string and out_type and out_class_type
      | Octy_signature of option out_type and list out_class_sig_item ]
    and out_class_sig_item =
      [ Ocsg_constraint of out_type and out_type
      | Ocsg_method of string and bool and bool and out_type
      | Ocsg_value of string and bool and bool and out_type ]
    ;
    type out_module_type =
      [ Omty_abstract
      | Omty_functor of
          option (option string * out_module_type) and out_module_type
      | Omty_ident of out_ident
      | Omty_signature of list out_sig_item
      | Omty_alias of out_ident ]
    and out_sig_item =
      [ Osig_class of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_class_type of
          bool and string and list (string * (bool * bool)) and
            out_class_type and out_rec_status
      | Osig_typext of out_extension_constructor and out_ext_status
      | Osig_modtype of string and out_module_type
      | Osig_module of string and out_module_type and out_rec_status
      | Osig_type of out_type_decl and out_rec_status
      | Osig_value of out_val_decl
      | Osig_ellipsis ]
    and out_type_decl =
      { otype_name : string;
        otype_params : list (string * (bool * bool));
        otype_type : out_type;
        otype_private : Asttypes.private_flag;
        otype_immediate : Type_immediacy.t;
        otype_unboxed : bool;
        otype_cstrs : list (out_type * out_type) }
    and out_extension_constructor =
      { oext_name : string;
        oext_type_name : string;
        oext_type_params : list string;
        oext_args : list out_type;
        oext_ret_type : option out_type;
        oext_private : Asttypes.private_flag }
    and out_type_extension =
      { otyext_name : string;
        otyext_params : list string;
        otyext_constructors : list (string * list out_type * option out_type);
        otyext_private : Asttypes.private_flag }
    and out_val_decl =
      { oval_name : string;
        oval_type : out_type;
        oval_prims : list string;
        oval_attributes : list out_attribute }
    and out_rec_status = [ Orec_not | Orec_first | Orec_next ]
    and out_ext_status = [ Oext_first | Oext_next | Oext_exception ];
    type out_phrase =
      [ Ophr_eval of out_value and out_type
      | Ophr_signature of list (out_sig_item * option out_value)
      | Ophr_exception of (exn * out_value) ]
    ;
  end
;
# 40 "all_ast.ORIG.ml"
end;
