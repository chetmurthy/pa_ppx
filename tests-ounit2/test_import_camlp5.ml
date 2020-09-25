open OUnit2

module T : sig

module Ploc : sig
include (module type of Ploc with type t = Ploc.t)

val pp : t Fmt.t
type 'a vala = [%import: 'a Ploc.vala] [@@deriving show]
end

[%%import: MLast.expr
    [@add type loc = [%import: MLast.loc]
          and type_var = [%import: MLast.type_var]
          and 'a vala = [%import: 'a Ploc.vala]
    ]
] [@@deriving show]

  end = struct
module Ploc= struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show]
end

[%%import: MLast.expr
    [@add type loc = [%import: MLast.loc]
          and type_var = [%import: MLast.type_var]
          and 'a vala = [%import: 'a Ploc.vala]
    ]
] [@@deriving show]
end

module T2 = struct
module Ploc= struct
include Ploc

let pp ppf x = Fmt.(const string "<loc>" ppf ())
end

[%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@add [%%import: 'a Ploc.vala]]
    [@with Ploc.vala := vala]
] [@@deriving show]

end

let test_simplest ctxt =
  ()

let suite = "Test import_camlp5" >::: [
    "test_simplest"   >:: test_simplest
  ]
let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
