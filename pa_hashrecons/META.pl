#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,fmt,pa_ppx_base"
version = "$version"
description = "pa_ppx pa_hashrecons support"
directory = "$destdir/pa_ppx_pa_hashrecons"

# For linking
archive(byte) = "pa_ppx_pa_hashrecons.cma"
archive(native) = "pa_ppx_pa_hashrecons.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_pa_hashrecons.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor) = "pa_ppx_pa_hashrecons.cma"
)

EOF
