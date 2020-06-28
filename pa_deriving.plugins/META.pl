#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_deriving_plugins" preprocessor:
requires = "camlp5,pa_ppx_deriving"
version = "$version"
description = "pa_ppx_deriving_plugins support"

package "std" (
# For linking
  package "link" (
requires = "camlp5,pa_ppx_deriving.link"
archive(byte) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
archive(native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx pa_deriving_iter.cmx pa_deriving_map.cmx pa_deriving_fold.cmx pa_deriving_make.cmx"
  )
  requires = "pa_ppx_runtime"

# For the toploop:
requires(toploop) = "camlp5,pa_ppx_deriving"
archive(byte,toploop) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
  archive(syntax,preprocessor,-native) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
  archive(syntax,preprocessor,native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx pa_deriving_iter.cmx pa_deriving_map.cmx pa_deriving_fold.cmx pa_deriving_make.cmx"

)

package "show" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_show.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_show.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_show.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_show.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "eq" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_eq.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_eq.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_eq.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "ord" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_ord.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_ord.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_ord.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "enum" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_enum.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_enum.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_enum.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "iter" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_iter.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_iter.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_iter.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "map" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_map.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_map.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_map.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "fold" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_fold.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_fold.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_fold.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "make" (
  requires(toploop) = "camlp5,pa_ppx_deriving"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_make.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_make.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_make.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "yojson" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_yojson.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_yojson.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_yojson.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_yojson.cmo"
  )
  requires = "pa_ppx_runtime"
)

package "sexp" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_sexp.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_sexp.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_sexp.cmo"
  )
  requires = "pa_ppx_runtime"
)


package "protobuf" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_protobuf.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_protobuf.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_protobuf.cmx"

  package "link" (
  requires(byte) = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_protobuf.cmo"
  )
  requires = "pa_ppx_runtime,pa_ppx_protobuf_runtime"
)

EOF
