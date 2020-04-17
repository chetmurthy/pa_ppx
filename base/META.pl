#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,fmt"
version = "$version"
description = "pa_ppx base support"
directory = "$destdir/pa_ppx_base"

# For linking
archive(byte) = "pa_ppx_base.cma"
archive(native) = "pa_ppx_base.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_base.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt"
  archive(syntax,preprocessor) = "pa_ppx_base.cma"
)

EOF
