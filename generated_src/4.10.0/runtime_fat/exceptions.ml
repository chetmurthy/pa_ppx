

type t = exn == ..[@@"deriving_inline" (show, sexp, yojson, eq);]; [@@@"ocaml.text" "/*";]; module M_equal =
  struct
    type nonrec equal = { f : mutable t → t → Stdlib.Bool.t };
    value f = {f _ _ = False};
  end
; [@@@"ocaml.text" "/*";]; value equal x = M_equal.f.M_equal.f x;

[@@@"ocaml.text" "/*";];

module M_to_yojson =
  struct
    type nonrec to_yojson = { f : mutable t → Yojson.Safe.t };
    value f =
      {f _ =
        invalid_arg
          ("to_yojson: Maybe a [@@deriving yojson] is missing when extending the type " ^
           "t")}
    ;
  end
;

[@@@"ocaml.text" "/*";];

value to_yojson x = M_to_yojson.f.M_to_yojson.f x;

[@@@"ocaml.text" "/*";];

module M_of_yojson =
  struct
    type nonrec of_yojson =
      { f : mutable Yojson.Safe.t → Rresult.result t string }
    ;
    value f =
      {f _ =
        invalid_arg
          ("of_yojson: Maybe a [@@deriving yojson] is missing when extending the type " ^
           "t")}
    ;
  end
;

[@@@"ocaml.text" "/*";];

value of_yojson x = M_of_yojson.f.M_of_yojson.f x;

[@@@"ocaml.text" "/*";];

module M_sexp_of_t =
  struct
    type nonrec sexp_of_t = { f : mutable t → Sexplib.Sexp.t };
    value f =
      {f _ =
        invalid_arg
          ("sexp_of_t: Maybe a [@@deriving sexp] is missing when extending the type " ^
           "t")}
    ;
  end
;

[@@@"ocaml.text" "/*";];

value sexp_of_t x = M_sexp_of_t.f.M_sexp_of_t.f x;

[@@@"ocaml.text" "/*";];

module M_t_of_sexp =
  struct
    type nonrec t_of_sexp = { f : mutable Sexplib.Sexp.t → t };
    value f =
      {f _ =
        invalid_arg
          ("t_of_sexp: Maybe a [@@deriving sexp] is missing when extending the type " ^
           "t")}
    ;
  end
;

[@@@"ocaml.text" "/*";];

value t_of_sexp x = M_t_of_sexp.f.M_t_of_sexp.f x; [@@@"ocaml.text" "/*";]; module M_pp =
  struct
    type nonrec pp = { f : mutable Fmt.t t };
    value f =
      {f _ =
        invalid_arg
          ("pp: Maybe a [@@deriving show] is missing when extending the type " ^
           "t")}
    ;
  end
; [@@@"ocaml.text" "/*";]; value pp x = M_pp.f.M_pp.f x; value show arg = Format.asprintf "%a" M_pp.f.M_pp.f arg;

[@@@"end"];

type t +=
  [ Help = Arg.Help[@"name" "Arg.Help";]
  | Bad = Arg.Bad[@"name" "Arg.Bad";]
  | Finally_raised = Fun.Finally_raised[@"name" "Fun.Finally_raised";]
  | Undefined = Lazy.Undefined[@"name" "Lazy.Undefined";]
  | Parse_error = Parsing.Parse_error[@"name" "Parsing.Parse_error";]
  | QueueEmpty = Queue.Empty[@"name" "Queue.Empty";]
  | Scan_failure = Scanf.Scan_failure[@"name" "Scanf.Scan_failure";]
  | StackEmpty = Stack.Empty[@"name" "Stack.Empty";]
  | Exit = Stdlib.Exit[@"name" "Stdlib.Exit";]
  | Match_failure = Stdlib.Match_failure[@"name" "Stdlib.Match_failure";]
  | Assert_failure = Stdlib.Assert_failure[@"name" "Stdlib.Assert_failure";]
  | Invalid_argument
    = Stdlib.Invalid_argument[@"name" "Stdlib.Invalid_argument";]
  | Failure = Stdlib.Failure[@"name" "Stdlib.Failure";]
  | Not_found = Stdlib.Not_found[@"name" "Stdlib.Not_found";]
  | Out_of_memory = Stdlib.Out_of_memory[@"name" "Stdlib.Out_of_memory";]
  | Stack_overflow = Stdlib.Stack_overflow[@"name" "Stdlib.Stack_overflow";]
  | Sys_error = Stdlib.Sys_error[@"name" "Stdlib.Sys_error";]
  | End_of_file = Stdlib.End_of_file[@"name" "Stdlib.End_of_file";]
  | Division_by_zero
    = Stdlib.Division_by_zero[@"name" "Stdlib.Division_by_zero";]
  | Sys_blocked_io = Stdlib.Sys_blocked_io[@"name" "Stdlib.Sys_blocked_io";]
  | Undefined_recursive_module
    = Stdlib.Undefined_recursive_module[@"name" "Stdlib.Undefined_recursive_module";]
  | StreamFailure = Stream.Failure[@"name" "Stream.Failure";]
  | Error = Stream.Error[@"name" "Stream.Error";]
  | Break = Sys.Break[@"name" "Sys.Break";] ][@@"deriving_inline" (show, sexp, yojson, eq);]
;

let open M_equal in
let fallback = f.f in
f.f :=
  fun a b →
    match (a, b) with
    [ (Help a_0, Help b_0) → (fun a b → a = b) a_0 b_0
    | (Bad a_0, Bad b_0) → (fun a b → a = b) a_0 b_0
    | (Finally_raised a_0, Finally_raised b_0) → equal a_0 b_0
    | (Undefined, Undefined) → True
    | (Parse_error, Parse_error) → True
    | (QueueEmpty, QueueEmpty) → True
    | (Scan_failure a_0, Scan_failure b_0) → (fun a b → a = b) a_0 b_0
    | (StackEmpty, StackEmpty) → True
    | (Exit, Exit) → True
    | (Match_failure a_0, Match_failure b_0) →
        (fun (a_0, a_1, a_2) (b_0, b_1, b_2) →
           ((fun a b → a = b) a_0 b_0 && (fun a b → a = b) a_1 b_1) &&
           (fun a b → a = b) a_2 b_2)
          a_0 b_0
    | (Assert_failure a_0, Assert_failure b_0) →
        (fun (a_0, a_1, a_2) (b_0, b_1, b_2) →
           ((fun a b → a = b) a_0 b_0 && (fun a b → a = b) a_1 b_1) &&
           (fun a b → a = b) a_2 b_2)
          a_0 b_0
    | (Invalid_argument a_0, Invalid_argument b_0) → (fun a b → a = b) a_0 b_0
    | (Failure a_0, Failure b_0) → (fun a b → a = b) a_0 b_0
    | (Not_found, Not_found) → True
    | (Out_of_memory, Out_of_memory) → True
    | (Stack_overflow, Stack_overflow) → True
    | (Sys_error a_0, Sys_error b_0) → (fun a b → a = b) a_0 b_0
    | (End_of_file, End_of_file) → True
    | (Division_by_zero, Division_by_zero) → True
    | (Sys_blocked_io, Sys_blocked_io) → True
    | (Undefined_recursive_module a_0, Undefined_recursive_module b_0) →
        (fun (a_0, a_1, a_2) (b_0, b_1, b_2) →
           ((fun a b → a = b) a_0 b_0 && (fun a b → a = b) a_1 b_1) &&
           (fun a b → a = b) a_2 b_2)
          a_0 b_0
    | (StreamFailure, StreamFailure) → True
    | (Error a_0, Error b_0) → (fun a b → a = b) a_0 b_0
    | (Break, Break) → True
    | (a, b) → fallback a b ];

let open M_to_yojson in
let fallback = f.f in
f.f :=
  fun
  [ Help v0 → `List [`String "Arg.Help"; Runtime.Yojson.string_to_yojson v0]
  | Bad v0 → `List [`String "Arg.Bad"; Runtime.Yojson.string_to_yojson v0]
  | Finally_raised v0 → `List [`String "Fun.Finally_raised"; to_yojson v0]
  | Undefined → `List [`String "Lazy.Undefined"]
  | Parse_error → `List [`String "Parsing.Parse_error"]
  | QueueEmpty → `List [`String "Queue.Empty"]
  | Scan_failure v0 →
      `List [`String "Scanf.Scan_failure"; Runtime.Yojson.string_to_yojson v0]
  | StackEmpty → `List [`String "Stack.Empty"]
  | Exit → `List [`String "Stdlib.Exit"]
  | Match_failure v0 →
      `List
        [`String "Stdlib.Match_failure";
         (fun (v0, v1, v2) →
            `List
              [Runtime.Yojson.string_to_yojson v0;
               Runtime.Yojson.int_to_yojson v1;
               Runtime.Yojson.int_to_yojson v2])
           v0]
  | Assert_failure v0 →
      `List
        [`String "Stdlib.Assert_failure";
         (fun (v0, v1, v2) →
            `List
              [Runtime.Yojson.string_to_yojson v0;
               Runtime.Yojson.int_to_yojson v1;
               Runtime.Yojson.int_to_yojson v2])
           v0]
  | Invalid_argument v0 →
      `List
        [`String "Stdlib.Invalid_argument";
         Runtime.Yojson.string_to_yojson v0]
  | Failure v0 →
      `List [`String "Stdlib.Failure"; Runtime.Yojson.string_to_yojson v0]
  | Not_found → `List [`String "Stdlib.Not_found"]
  | Out_of_memory → `List [`String "Stdlib.Out_of_memory"]
  | Stack_overflow → `List [`String "Stdlib.Stack_overflow"]
  | Sys_error v0 →
      `List [`String "Stdlib.Sys_error"; Runtime.Yojson.string_to_yojson v0]
  | End_of_file → `List [`String "Stdlib.End_of_file"]
  | Division_by_zero → `List [`String "Stdlib.Division_by_zero"]
  | Sys_blocked_io → `List [`String "Stdlib.Sys_blocked_io"]
  | Undefined_recursive_module v0 →
      `List
        [`String "Stdlib.Undefined_recursive_module";
         (fun (v0, v1, v2) →
            `List
              [Runtime.Yojson.string_to_yojson v0;
               Runtime.Yojson.int_to_yojson v1;
               Runtime.Yojson.int_to_yojson v2])
           v0]
  | StreamFailure → `List [`String "Stream.Failure"]
  | Error v0 →
      `List [`String "Stream.Error"; Runtime.Yojson.string_to_yojson v0]
  | Break → `List [`String "Sys.Break"]
  | z → fallback z ];

let open M_of_yojson in
let fallback = f.f in
f.f :=
  fun
  [ `List [`String "Arg.Help"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Help v0))
  | `List [`String "Arg.Bad"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Bad v0))
  | `List [`String "Fun.Finally_raised"; v0] →
      Rresult.R.bind (of_yojson v0) (fun v0 → Result.Ok (Finally_raised v0))
  | `List [`String "Lazy.Undefined"] → Result.Ok Undefined
  | `List [`String "Parsing.Parse_error"] → Result.Ok Parse_error
  | `List [`String "Queue.Empty"] → Result.Ok QueueEmpty
  | `List [`String "Scanf.Scan_failure"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Scan_failure v0))
  | `List [`String "Stack.Empty"] → Result.Ok StackEmpty
  | `List [`String "Stdlib.Exit"] → Result.Ok Exit
  | `List [`String "Stdlib.Match_failure"; v0] →
      Rresult.R.bind
        ((fun
          [ `List [v0; v1; v2] →
              Rresult.R.bind
                (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
                (fun v0 →
                   Rresult.R.bind
                     (Runtime.Yojson.int_of_yojson "<longid_lident>" v1)
                     (fun v1 →
                        Rresult.R.bind
                          (Runtime.Yojson.int_of_yojson "<longid_lident>" v2)
                          (fun v2 → Result.Ok (v0, v1, v2))))
          | _ → Result.Error "<longid_lident>" ])
           v0)
        (fun v0 → Result.Ok (Match_failure v0))
  | `List [`String "Stdlib.Assert_failure"; v0] →
      Rresult.R.bind
        ((fun
          [ `List [v0; v1; v2] →
              Rresult.R.bind
                (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
                (fun v0 →
                   Rresult.R.bind
                     (Runtime.Yojson.int_of_yojson "<longid_lident>" v1)
                     (fun v1 →
                        Rresult.R.bind
                          (Runtime.Yojson.int_of_yojson "<longid_lident>" v2)
                          (fun v2 → Result.Ok (v0, v1, v2))))
          | _ → Result.Error "<longid_lident>" ])
           v0)
        (fun v0 → Result.Ok (Assert_failure v0))
  | `List [`String "Stdlib.Invalid_argument"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Invalid_argument v0))
  | `List [`String "Stdlib.Failure"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Failure v0))
  | `List [`String "Stdlib.Not_found"] → Result.Ok Not_found
  | `List [`String "Stdlib.Out_of_memory"] → Result.Ok Out_of_memory
  | `List [`String "Stdlib.Stack_overflow"] → Result.Ok Stack_overflow
  | `List [`String "Stdlib.Sys_error"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Sys_error v0))
  | `List [`String "Stdlib.End_of_file"] → Result.Ok End_of_file
  | `List [`String "Stdlib.Division_by_zero"] → Result.Ok Division_by_zero
  | `List [`String "Stdlib.Sys_blocked_io"] → Result.Ok Sys_blocked_io
  | `List [`String "Stdlib.Undefined_recursive_module"; v0] →
      Rresult.R.bind
        ((fun
          [ `List [v0; v1; v2] →
              Rresult.R.bind
                (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
                (fun v0 →
                   Rresult.R.bind
                     (Runtime.Yojson.int_of_yojson "<longid_lident>" v1)
                     (fun v1 →
                        Rresult.R.bind
                          (Runtime.Yojson.int_of_yojson "<longid_lident>" v2)
                          (fun v2 → Result.Ok (v0, v1, v2))))
          | _ → Result.Error "<longid_lident>" ])
           v0)
        (fun v0 → Result.Ok (Undefined_recursive_module v0))
  | `List [`String "Stream.Failure"] → Result.Ok StreamFailure
  | `List [`String "Stream.Error"; v0] →
      Rresult.R.bind (Runtime.Yojson.string_of_yojson "<longid_lident>" v0)
        (fun v0 → Result.Ok (Error v0))
  | `List [`String "Sys.Break"] → Result.Ok Break
  | z → fallback z ];

let open M_sexp_of_t in
let fallback = f.f in
f.f :=
  fun
  [ Help v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Arg.Help"; Sexplib0.Sexp_conv.sexp_of_string v0]
  | Bad v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Arg.Bad"; Sexplib0.Sexp_conv.sexp_of_string v0]
  | Finally_raised v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Fun.Finally_raised"; sexp_of_t v0]
  | Undefined → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Lazy.Undefined"]
  | Parse_error →
      Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Parsing.Parse_error"]
  | QueueEmpty → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Queue.Empty"]
  | Scan_failure v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Scanf.Scan_failure";
         Sexplib0.Sexp_conv.sexp_of_string v0]
  | StackEmpty → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stack.Empty"]
  | Exit → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Exit"]
  | Match_failure v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Match_failure";
         (fun (v0, v1, v2) →
            Sexplib0.Sexp.List
              [Sexplib0.Sexp_conv.sexp_of_string v0;
               Sexplib0.Sexp_conv.sexp_of_int v1;
               Sexplib0.Sexp_conv.sexp_of_int v2])
           v0]
  | Assert_failure v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Assert_failure";
         (fun (v0, v1, v2) →
            Sexplib0.Sexp.List
              [Sexplib0.Sexp_conv.sexp_of_string v0;
               Sexplib0.Sexp_conv.sexp_of_int v1;
               Sexplib0.Sexp_conv.sexp_of_int v2])
           v0]
  | Invalid_argument v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Invalid_argument";
         Sexplib0.Sexp_conv.sexp_of_string v0]
  | Failure v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Failure";
         Sexplib0.Sexp_conv.sexp_of_string v0]
  | Not_found → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Not_found"]
  | Out_of_memory →
      Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Out_of_memory"]
  | Stack_overflow →
      Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Stack_overflow"]
  | Sys_error v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Sys_error";
         Sexplib0.Sexp_conv.sexp_of_string v0]
  | End_of_file → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.End_of_file"]
  | Division_by_zero →
      Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Division_by_zero"]
  | Sys_blocked_io →
      Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Sys_blocked_io"]
  | Undefined_recursive_module v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stdlib.Undefined_recursive_module";
         (fun (v0, v1, v2) →
            Sexplib0.Sexp.List
              [Sexplib0.Sexp_conv.sexp_of_string v0;
               Sexplib0.Sexp_conv.sexp_of_int v1;
               Sexplib0.Sexp_conv.sexp_of_int v2])
           v0]
  | StreamFailure → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stream.Failure"]
  | Error v0 →
      Sexplib0.Sexp.List
        [Sexplib0.Sexp.Atom "Stream.Error";
         Sexplib0.Sexp_conv.sexp_of_string v0]
  | Break → Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Sys.Break"]
  | z → fallback z ];

let open M_t_of_sexp in
let fallback = f.f in
f.f :=
  fun
  [ Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Arg.Help"; v0] →
      Help (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Arg.Bad"; v0] →
      Bad (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Fun.Finally_raised"; v0] →
      Finally_raised (t_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Lazy.Undefined"] → Undefined
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Parsing.Parse_error"] →
      Parse_error
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Queue.Empty"] → QueueEmpty
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Scanf.Scan_failure"; v0] →
      Scan_failure (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stack.Empty"] → StackEmpty
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Exit"] → Exit
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Match_failure"; v0] →
      Match_failure
        ((fun
          [ Sexplib0.Sexp.List [v0; v1; v2] →
              (Sexplib0.Sexp_conv.string_of_sexp v0,
               Sexplib0.Sexp_conv.int_of_sexp v1,
               Sexplib0.Sexp_conv.int_of_sexp v2)
          | _ → failwith "wrong number of members in list" ])
           v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Assert_failure"; v0] →
      Assert_failure
        ((fun
          [ Sexplib0.Sexp.List [v0; v1; v2] →
              (Sexplib0.Sexp_conv.string_of_sexp v0,
               Sexplib0.Sexp_conv.int_of_sexp v1,
               Sexplib0.Sexp_conv.int_of_sexp v2)
          | _ → failwith "wrong number of members in list" ])
           v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Invalid_argument"; v0] →
      Invalid_argument (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Failure"; v0] →
      Failure (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Not_found"] → Not_found
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Out_of_memory"] →
      Out_of_memory
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Stack_overflow"] →
      Stack_overflow
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Sys_error"; v0] →
      Sys_error (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.End_of_file"] → End_of_file
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Division_by_zero"] →
      Division_by_zero
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stdlib.Sys_blocked_io"] →
      Sys_blocked_io
  | Sexplib0.Sexp.List
      [Sexplib0.Sexp.Atom "Stdlib.Undefined_recursive_module"; v0] →
      Undefined_recursive_module
        ((fun
          [ Sexplib0.Sexp.List [v0; v1; v2] →
              (Sexplib0.Sexp_conv.string_of_sexp v0,
               Sexplib0.Sexp_conv.int_of_sexp v1,
               Sexplib0.Sexp_conv.int_of_sexp v2)
          | _ → failwith "wrong number of members in list" ])
           v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stream.Failure"] → StreamFailure
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Stream.Error"; v0] →
      Error (Sexplib0.Sexp_conv.string_of_sexp v0)
  | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "Sys.Break"] → Break
  | z → fallback z ];

let open M_pp in
let fallback = f.f in
f.f :=
  fun ofmt →
    fun
    [ Help v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Arg.Help@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Bad v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Arg.Bad@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Finally_raised v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Fun.Finally_raised@ %a)@]" pp v0
    | Undefined → let open Runtime.Fmt in pf ofmt "@[<2>Lazy.Undefined@]"
    | Parse_error →
        let open Runtime.Fmt in pf ofmt "@[<2>Parsing.Parse_error@]"
    | QueueEmpty → let open Runtime.Fmt in pf ofmt "@[<2>Queue.Empty@]"
    | Scan_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Scanf.Scan_failure@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | StackEmpty → let open Runtime.Fmt in pf ofmt "@[<2>Stack.Empty@]"
    | Exit → let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Exit@]"
    | Match_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Match_failure@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | Assert_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Assert_failure@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | Invalid_argument v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Invalid_argument@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Failure@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Not_found → let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Not_found@]"
    | Out_of_memory →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Out_of_memory@]"
    | Stack_overflow →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Stack_overflow@]"
    | Sys_error v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Sys_error@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | End_of_file →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.End_of_file@]"
    | Division_by_zero →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Division_by_zero@]"
    | Sys_blocked_io →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Sys_blocked_io@]"
    | Undefined_recursive_module v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Undefined_recursive_module@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | StreamFailure → let open Runtime.Fmt in pf ofmt "@[<2>Stream.Failure@]"
    | Error v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stream.Error@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Break → let open Runtime.Fmt in pf ofmt "@[<2>Sys.Break@]"
    | z → fallback ofmt z ];

[@@@"end"];

value print_exn exn = Some (show exn);
Printexc.register_printer print_exn;


