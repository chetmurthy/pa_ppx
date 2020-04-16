#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_pa_deriving_plugins" preprocessor:
requires = "camlp5,pa_ppx_pa_deriving"
version = "$version"
description = "pa_ppx_pa_deriving_plugins support"
directory = "$destdir/pa_ppx_pa_deriving_plugins"

# For linking
archive(byte) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo"
archive(native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx"

# For the toploop:
archive(byte,toploop) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo"

# For the preprocessor itself:
archive(syntax,preprocessor) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo"

package "show" (
  requires(syntax,toploop) = "camlp5,pa_ppx_pa_deriving"
  archive(syntax,toploop) = "pa_deriving_show.cmo"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_pa_deriving"
    archive(syntax,preprocessor) = "pa_deriving_show.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_pa_deriving"
  archive(byte) = "pa_deriving_show.cmo"
)

package "eq" (
  requires(syntax,toploop) = "camlp5,pa_ppx_pa_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_pa_deriving"
    archive(syntax,preprocessor) = "pa_deriving_eq.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_pa_deriving"
  archive(byte) = "pa_deriving_eq.cmo"
)

package "ord" (
  requires(syntax,toploop) = "camlp5,pa_ppx_pa_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_pa_deriving"
    archive(syntax,preprocessor) = "pa_deriving_ord.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_pa_deriving"
  archive(byte) = "pa_deriving_ord.cmo"
)

package "enum" (
  requires(syntax,toploop) = "camlp5,pa_ppx_pa_deriving"

  package "syntax" (
    requires(syntax,preprocessor) = "camlp5,pa_ppx_pa_deriving"
    archive(syntax,preprocessor) = "pa_deriving_enum.cmo"
  )

  requires(byte) = "camlp5,pa_ppx_pa_deriving"
  archive(byte) = "pa_deriving_enum.cmo"
)

EOF
