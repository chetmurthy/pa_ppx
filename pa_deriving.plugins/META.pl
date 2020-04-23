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
requires = "camlp5,pa_ppx_deriving"
archive(byte) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
archive(native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx pa_deriving_iter.cmx pa_deriving_map.cmx pa_deriving_fold.cmx pa_deriving_make.cmx"

# For the toploop:
archive(byte,toploop) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"

  package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
  archive(syntax,preprocessor) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
  )

)

package "show" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"
  archive(syntax,toploop) = "pa_deriving_show.cmo"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_show.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_show.cmo"
)

package "eq" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_eq.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_eq.cmo"
)

package "ord" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_ord.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_ord.cmo"
)

package "enum" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_enum.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_enum.cmo"
)

package "iter" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_iter.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_iter.cmo"
)

package "map" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_map.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_map.cmo"
)

package "fold" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_fold.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_fold.cmo"
)

package "make" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_make.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_make.cmo"
)

package "yojson" (
  requires(syntax,toploop) = "camlp5,pa_ppx_deriving"
  archive(syntax,toploop) = "pa_deriving_yojson.cmo"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor) = "pa_deriving_yojson.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_deriving"
  archive(byte) = "pa_deriving_yojson.cmo"
)

EOF
