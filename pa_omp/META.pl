#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_omp" library:
requires = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
version = "$Version::version"
description = "pa_ppx_omp support"

# For linking
package "link" (
requires = "camlp5.pa_o.link,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
archive(byte) = "pa_omp.cma"
archive(native) = "pa_omp.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_omp.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
  archive(syntax,preprocessor,-native) = "pa_omp.cma"
  archive(syntax,preprocessor,native) = "pa_omp.cmxa"

EOF
