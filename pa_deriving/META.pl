#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_pa_deriving" preprocessor:
version = "$version"
description = "pa_ppx_pa_deriving base support"
directory = "$destdir/pa_ppx_pa_deriving"

# For linking
requires = "camlp5,fmt,pa_ppx_base"
archive(byte) = "pa_deriving.cma"
archive(native) = "pa_deriving.cmxa"

# For the toploop:
requires(byte,toploop) = "camlp5,fmt,pa_ppx_base"
archive(byte,toploop) = "pa_deriving.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor) = "pa_deriving.cma"
)
EOF
