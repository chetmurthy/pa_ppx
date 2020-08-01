module ParamMap
  (ARG : sig value arg_ctyp_f : Ploc.t → MLast.ctyp → MLast.ctyp; end) :
  sig
    type param_t = { type_id : string; arg_id : string };
    value type_id : param_t → string;
    value arg_id : param_t → string;
    value param_ctyp : ?mono:bool → MLast.loc → param_t → MLast.ctyp;
    value arg_ctyp : ?mono:bool → MLast.loc → param_t → MLast.ctyp;
    value arg_expr : MLast.loc → param_t → MLast.expr;
    value arg_patt :
      ?naked:bool → ?mono:bool → MLast.loc → param_t → MLast.patt;
    value find : string → list param_t → param_t;
    type t = list param_t;
    value make :
      string → Ploc.t → list (Ploc.vala (option string) * α) → list param_t;
    value make_of_ids : list string → list param_t;
    value quantify_over_ctyp : list param_t → MLast.ctyp → MLast.ctyp;
    value wrap_type_constraints :
      MLast.loc → list param_t → list (string * α) →
        list (string * MLast.ctyp) →
        list
          (MLast.patt * α *
           Ploc.vala
             (list
                (Ploc.vala (Ploc.vala (MLast.loc * string) * MLast.payload))));
  end
;
value monomorphize_ctyp : MLast.ctyp → MLast.ctyp;
value is_type_abbreviation : MLast.ctyp → bool;
value type_params : MLast.ctyp → list string;
value extract_allowed_attribute_expr :
  Pa_ppx_base.Pa_passthru.Ctxt.t → (string * string) →
    list (Ploc.vala (Ploc.vala (α * string) * MLast.payload)) →
    option MLast.expr;
