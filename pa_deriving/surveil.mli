module DerivingConfig :
  sig
    value addset : ref (list α) → α → unit;
    value addsetl : ref (list α) → list α → unit;
    type form_t = [ Short | Medium | Long ];
    type t =
      { all_plugins : ref (list string);
        all_attributes : ref (list string);
        current_plugins : ref (list string);
        current_attributes : ref (list string);
        allowed_form : ref (option form_t) }
    ;
    value mk : unit → t;
    type Pa_ppx_base.Pa_passthru.scratchdata_t +=
      [ Pa_deriving of t ]
    ;
    value get : Pa_ppx_base.Pa_passthru.Ctxt.t → t;
    value init : Pa_ppx_base.Pa_passthru.Ctxt.t → unit;
    value legitimate_plugin_reference :
      α → (string * list (string * MLast.expr)) → bool;
    value start_decl :
      Ploc.t → t → list (string * list (string * MLast.expr)) →
        list (string * list (string * MLast.expr));
    value end_decl : t → list string;
    value set_form : t → form_t → unit;
    value get_form : t → form_t;
    value dump : Fmt.t t;
    value allowed_attribute : t → string → string → string;
    value is_allowed_attribute :
      t → string → string → Ploc.vala (Ploc.vala (α * string) * β) → bool;
  end
;
module alias DC = DerivingConfig;
value install : unit → unit;
