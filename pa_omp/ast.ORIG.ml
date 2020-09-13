(* camlp5r *)
(* pp_parsetree.ml,v *)

module Lexing = struct
[%%import: PREFIX Lexing.position] 
end

#if MAJOR_VERSION = 4 && MINOR_VERSION > 5
module Warnings = struct
[%%import: PREFIX Warnings.loc] 
end
#endif

module Location = struct
[%%import: PREFIX Location.t] 
[%%import: 'a PREFIX Location.loc] 
end
module Longident = struct
[%%import: PREFIX Longident.t] 
end
module Asttypes = struct
[%%import: PREFIX Asttypes.loc] 
#if MAJOR_VERSION = 4 && MINOR_VERSION >= 3
[%%import: PREFIX Asttypes.arg_label] 
#endif
[%%import: PREFIX Asttypes.label] 
[%%import: PREFIX Asttypes.closed_flag] 
[%%import: PREFIX Asttypes.rec_flag] 
[%%import: PREFIX Asttypes.direction_flag] 
[%%import: PREFIX Asttypes.private_flag] 
[%%import: PREFIX Asttypes.mutable_flag] 
[%%import: PREFIX Asttypes.virtual_flag] 
[%%import: PREFIX Asttypes.override_flag] 
[%%import: PREFIX Asttypes.variance] 
#if MAJOR_VERSION = 4 && MINOR_VERSION < 3
[%%import: PREFIX Asttypes.constant] 
#endif
end
module Parsetree = struct
open Asttypes
#if MAJOR_VERSION = 4 && MINOR_VERSION >= 3
[%%import: PREFIX Parsetree.constant] 
#endif
type location_stack = Location.t list 
[%%import: PREFIX Parsetree.attribute] 
end

#if MAJOR_VERSION = 4 && MINOR_VERSION >= 10
module Type_immediacy = struct
[%%import: PREFIX Type_immediacy.t] 
end
#endif

module Outcometree = struct
#if MAJOR_VERSION = 4 && MINOR_VERSION > 7
[%%import: PREFIX Outcometree.out_name] 
#endif
[%%import: PREFIX Outcometree.out_ident] 
#if MAJOR_VERSION = 4 && MINOR_VERSION > 5
[%%import: PREFIX Outcometree.out_string] 
#endif
#if MAJOR_VERSION = 4 && MINOR_VERSION > 2
[%%import: PREFIX Outcometree.out_attribute] 
#endif
[%%import: PREFIX Outcometree.out_value] 
[%%import: PREFIX Outcometree.out_type] 
[%%import: PREFIX Outcometree.out_class_type] 
[%%import: PREFIX Outcometree.out_module_type] 
[%%import: PREFIX Outcometree.out_phrase] 
end
