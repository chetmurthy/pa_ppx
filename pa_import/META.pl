#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_import" preprocessor:
requires = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
version = "$version"
description = "pa_ppx_import base support"

# For linking
archive(byte) = "pa_import.cma"
archive(native) = "pa_import.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_import.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
  archive(syntax,preprocessor) = "pa_import.cma"
)

EOF
