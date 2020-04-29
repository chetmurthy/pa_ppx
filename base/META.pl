#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,rresult,fmt,pa_ppx_utils,compiler-libs.common"
version = "$version"
description = "pa_ppx base support"

# For linking
archive(byte) = "pa_ppx_base.cma"
archive(native) = "pa_ppx_base.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_base.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,rresult,fmt,pa_ppx_utils,compiler-libs.common"
  archive(syntax,preprocessor) = "pa_ppx_base.cma"
)

EOF
