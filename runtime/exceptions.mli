(* camlp5o *)
(* pp_MLast.ml,v *)

declare
  declare
    type t = exn == ..[@@"deriving_inline" (show, sexp, yojson, eq);];
    declare
      module M_equal :
        sig
          type nonrec equal = { f : mutable t → t → Stdlib.Bool.t };
          value f : equal;
        end
      ;
      value equal : t → t → Stdlib.Bool.t;
    end;
    declare
      module M_to_yojson :
        sig
          type nonrec to_yojson = { f : mutable t → Yojson.Safe.t };
          value f : to_yojson;
        end
      ;
      value to_yojson : t → Yojson.Safe.t;
      module M_of_yojson :
        sig
          type nonrec of_yojson =
            { f : mutable Yojson.Safe.t → Rresult.result t string }
          ;
          value f : of_yojson;
        end
      ;
      value of_yojson : Yojson.Safe.t → Rresult.result t string;
    end;
    declare
      module M_sexp_of_t :
        sig
          type nonrec sexp_of_t = { f : mutable t → Sexplib.Sexp.t };
          value f : sexp_of_t;
        end
      ;
      value sexp_of_t : t → Sexplib.Sexp.t;
      module M_t_of_sexp :
        sig
          type nonrec t_of_sexp = { f : mutable Sexplib.Sexp.t → t };
          value f : t_of_sexp;
        end
      ;
      value t_of_sexp : Sexplib.Sexp.t → t;
    end;
    declare
      module M_pp :
        sig
          type nonrec pp = { f : mutable Fmt.t t };
          value f : pp;
        end
      ;
      value pp : Fmt.t t;
      value show : t → Stdlib.String.t;
    end;
    [@@@"end"];
  end;
  declare
    type t +=
      [ Help = Arg.Help | Bad = Arg.Bad | Finally_raised = Fun.Finally_raised | Undefined = Lazy.Undefined | Parse_error = Parsing.Parse_error | QueueEmpty = Queue.Empty | Scan_failure = Scanf.Scan_failure | StackEmpty = Stack.Empty | Exit = Stdlib.Exit | Match_failure = Stdlib.Match_failure | Assert_failure = Stdlib.Assert_failure | Invalid_argument = Stdlib.Invalid_argument | Failure = Stdlib.Failure | Not_found = Stdlib.Not_found | Out_of_memory = Stdlib.Out_of_memory | Stack_overflow = Stdlib.Stack_overflow | Sys_error = Stdlib.Sys_error | End_of_file = Stdlib.End_of_file | Division_by_zero = Stdlib.Division_by_zero | Sys_blocked_io = Stdlib.Sys_blocked_io | Undefined_recursive_module = Stdlib.Undefined_recursive_module | StreamFailure = Stream.Failure | Error = Stream.Error | Break = Sys.Break ][@@"deriving_inline" (show, sexp, yojson, eq);]
    ;
    declare end;
    declare end;
    declare end;
    declare end;
    [@@@"end"];
  end;
end;


