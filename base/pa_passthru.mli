value debug : ref bool;

type scratchdata_t = ..;
module rec EF :
  sig
    type extension_point α = Extfun.t α (Ctxt.t → (Ctxt.t -> α -> α) -> option α);
    type t =
      { ctyp : extension_point MLast.ctyp;
        generic_constructor : extension_point MLast.generic_constructor;
        patt : extension_point MLast.patt;
        case_branch : extension_point MLast.case_branch;
        expr : extension_point MLast.expr;
        module_type : extension_point MLast.module_type;
        signature : extension_point (list MLast.sig_item);
        sig_item : extension_point MLast.sig_item;
        with_constr : extension_point MLast.with_constr;
        longid : extension_point MLast.longid;
        module_expr : extension_point MLast.module_expr;
        structure : extension_point (list MLast.str_item);
        str_item : extension_point MLast.str_item;
        type_decl : extension_point MLast.type_decl;
        type_extension : extension_point MLast.type_extension;
        extension_constructor : extension_point MLast.extension_constructor;
        class_type : extension_point MLast.class_type;
        class_sig_item : extension_point MLast.class_sig_item;
        class_expr : extension_point MLast.class_expr;
        class_str_item : extension_point MLast.class_str_item;
        attribute_body : extension_point MLast.attribute_body;
        implem :
          extension_point (list (MLast.str_item * MLast.loc) * Pcaml.status);
        interf :
          extension_point (list (MLast.sig_item * MLast.loc) * Pcaml.status) ;
        top_phrase : extension_point (option MLast.str_item) ;
        use_file : extension_point (list MLast.str_item * bool)
      }
    ;
    value mk : unit → t;
  end
and Ctxt :
  sig
    type t =
      { filename : string;
        _module_path : list string;
        options : list (string * MLast.expr);
        ef : EF.t;
        scratch : list (string * scratchdata_t);
        refscratch : ref (list (string * scratchdata_t)) }
    ;
    value mk : EF.t → Ploc.t → t;
    value append_module : t → string → t;
    value module_path : t → list string;
    value module_path_s : t → string;
    value set_module_path : t → list string → t;
    value filename : t → string;
    value set_filename : t → string → t;
    value add_options : t → list (string * MLast.expr) → t;
    value option : t → string → MLast.expr;
    value scratchdata : t → string → scratchdata_t;
    value init_scratchdata : t → string → scratchdata_t → t;
    value update_scratchdata : t → string → scratchdata_t → t;
    value refscratchdata : t → string → scratchdata_t;
    value init_refscratchdata : t → string → scratchdata_t → unit;
  end
;
value ctyp : Ctxt.t → MLast.ctyp → MLast.ctyp;
value ctyp0 : Ctxt.t → MLast.ctyp → MLast.ctyp;
value generic_constructor :
  Ctxt.t → MLast.generic_constructor → MLast.generic_constructor;
value generic_constructor0 :
  Ctxt.t → MLast.generic_constructor → MLast.generic_constructor;
value poly_variant : Ctxt.t → MLast.poly_variant → MLast.poly_variant;
value patt : Ctxt.t → MLast.patt → MLast.patt;
value patt0 : Ctxt.t → MLast.patt → MLast.patt;
value expr : Ctxt.t → MLast.expr → MLast.expr;
value expr0 : Ctxt.t → MLast.expr → MLast.expr;
value case_branch : Ctxt.t → MLast.case_branch → MLast.case_branch;
value case_branch0 : Ctxt.t → MLast.case_branch → MLast.case_branch;
value module_type : Ctxt.t → MLast.module_type → MLast.module_type;
value module_type0 : Ctxt.t → MLast.module_type → MLast.module_type;
value signature : Ctxt.t → list MLast.sig_item → list MLast.sig_item;
value signature0 : Ctxt.t → list MLast.sig_item → list MLast.sig_item;
value sig_item : Ctxt.t → MLast.sig_item → MLast.sig_item;
value sig_item0 : Ctxt.t → MLast.sig_item → MLast.sig_item;
value with_constr : Ctxt.t → MLast.with_constr → MLast.with_constr;
value with_constr0 : Ctxt.t → MLast.with_constr → MLast.with_constr;
value longid : Ctxt.t → MLast.longid → MLast.longid;
value longid0 : Ctxt.t → MLast.longid → MLast.longid;
value module_expr : Ctxt.t → MLast.module_expr → MLast.module_expr;
value module_expr0 : Ctxt.t → MLast.module_expr → MLast.module_expr;
value structure : Ctxt.t → list MLast.str_item → list MLast.str_item;
value structure0 : Ctxt.t → list MLast.str_item → list MLast.str_item;
value str_item : Ctxt.t → MLast.str_item → MLast.str_item;
value str_item0 : Ctxt.t → MLast.str_item → MLast.str_item;
value type_decl : Ctxt.t → MLast.type_decl → MLast.type_decl;
value type_decl0 : Ctxt.t → MLast.type_decl → MLast.type_decl;
value type_extension : Ctxt.t → MLast.type_extension → MLast.type_extension;
value type_extension0 : Ctxt.t → MLast.type_extension → MLast.type_extension;
value extension_constructor :
  Ctxt.t → MLast.extension_constructor → MLast.extension_constructor;
value extension_constructor0 :
  Ctxt.t → MLast.extension_constructor → MLast.extension_constructor;
value class_type : Ctxt.t → MLast.class_type → MLast.class_type;
value class_type0 : Ctxt.t → MLast.class_type → MLast.class_type;
value class_sig_item : Ctxt.t → MLast.class_sig_item → MLast.class_sig_item;
value class_sig_item0 : Ctxt.t → MLast.class_sig_item → MLast.class_sig_item;
value class_expr : Ctxt.t → MLast.class_expr → MLast.class_expr;
value class_expr0 : Ctxt.t → MLast.class_expr → MLast.class_expr;
value class_str_item : Ctxt.t → MLast.class_str_item → MLast.class_str_item;
value class_str_item0 : Ctxt.t → MLast.class_str_item → MLast.class_str_item;
value longid_lident : Ctxt.t → MLast.longid_lident → MLast.longid_lident;
value attribute : Ctxt.t → MLast.attribute → MLast.attribute;
value attribute_body : Ctxt.t → MLast.attribute_body → MLast.attribute_body;
value attribute_body0 : Ctxt.t → MLast.attribute_body → MLast.attribute_body;
value attributes_no_anti :
  Ctxt.t → MLast.attributes_no_anti → MLast.attributes_no_anti;
value attributes : Ctxt.t → MLast.attributes → MLast.attributes;
value implem :
  Ctxt.t → (list (MLast.str_item * MLast.loc) * Pcaml.status) →
    (list (MLast.str_item * MLast.loc) * Pcaml.status);
value implem0 :
  Ctxt.t → (list (MLast.str_item * MLast.loc) * Pcaml.status) →
    (list (MLast.str_item * MLast.loc) * Pcaml.status);
value interf :
  Ctxt.t → (list (MLast.sig_item * MLast.loc) * Pcaml.status) →
    (list (MLast.sig_item * MLast.loc) * Pcaml.status);
value interf0 :
  Ctxt.t → (list (MLast.sig_item * MLast.loc) * Pcaml.status) →
    (list (MLast.sig_item * MLast.loc) * Pcaml.status);

type pass_t =
  { name : string;
    pass : option int;
    before : list string;
    after : list string;
    ef : EF.t }
;

value install : pass_t → unit;
