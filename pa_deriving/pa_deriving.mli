value extract_deriving0 :
  MLast.attribute → list (string * list (string * MLast.expr));
value is_deriving_attribute : MLast.attribute → bool;

module PI :
  sig
    type t =
      { name : string;
        alternates : list string;
        options : list string;
        alg_attributes : list string;
        expr_extensions : list string;
        ctyp_extensions : list string;
        expr : Pa_ppx_base.Pa_passthru.Ctxt.t → MLast.expr → MLast.expr;
        ctyp : Pa_ppx_base.Pa_passthru.Ctxt.t → MLast.ctyp → MLast.ctyp;
        str_item :
          string → Pa_ppx_base.Pa_passthru.Ctxt.t → MLast.str_item →
            MLast.str_item;
        sig_item :
          string → Pa_ppx_base.Pa_passthru.Ctxt.t → MLast.sig_item →
            MLast.sig_item;
        default_options : list (string * MLast.expr) }
    ;
    value attributes : t → list string;
    value is_medium_form_attribute :
      t → Ploc.vala (Ploc.vala (α * string) * β) → bool;
    value is_long_form_attribute :
      t → Ploc.vala (Ploc.vala (α * string) * β) → bool;
    value medium_form_attributes : t → list string;
    value long_form_attributes : t → list string;
  end
;

module Registry :
  sig
    value add : PI.t → unit;
    value mem : string → bool;
    value get : string → PI.t;
  end
;

value install : unit → unit;
