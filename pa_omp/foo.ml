(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: Lexing.position] 
end

#if MAJOR_VERSION = 4 && MINOR_VERSION > 5
module Warnings = struct
[%%import: Warnings.loc] 
end
#endif

module Location = struct
[%%import: Location.t] 
[%%import: 'a Location.loc] 
end
module Longident = struct
[%%import: Longident.t] 
end
module Asttypes = struct
[%%import: Asttypes.loc] 
#if MAJOR_VERSION = 4 && MINOR_VERSION >= 3
[%%import: Asttypes.arg_label] 
#endif
[%%import: Asttypes.label] 
[%%import: Asttypes.closed_flag] 
[%%import: Asttypes.rec_flag] 
[%%import: Asttypes.direction_flag] 
[%%import: Asttypes.private_flag] 
[%%import: Asttypes.mutable_flag] 
[%%import: Asttypes.virtual_flag] 
[%%import: Asttypes.override_flag] 
[%%import: Asttypes.variance] 
#if MAJOR_VERSION = 4 && MINOR_VERSION < 3
[%%import: Asttypes.constant] 
#endif
end
module Parsetree = struct
open Asttypes
#if MAJOR_VERSION = 4 && MINOR_VERSION >= 3
[%%import: Parsetree.constant] 
#endif
type location_stack = Location.t list 
[%%import: Parsetree.attribute] 
end

#if MAJOR_VERSION = 4 && MINOR_VERSION >= 10
module Type_immediacy = struct
[%%import: Type_immediacy.t] 
end
#endif

module Outcometree = struct
#if MAJOR_VERSION = 4 && MINOR_VERSION > 7
[%%import: Outcometree.out_name] 
#endif
[%%import: Outcometree.out_ident] 
#if MAJOR_VERSION = 4 && MINOR_VERSION > 5
[%%import: Outcometree.out_string] 
#endif
#if MAJOR_VERSION = 4 && MINOR_VERSION > 2
[%%import: Outcometree.out_attribute] 
#endif
[%%import: Outcometree.out_value] 
[%%import: Outcometree.out_type] 
[%%import: Outcometree.out_class_type] 
[%%import: Outcometree.out_module_type] 
[%%import: Outcometree.out_phrase] 
end
