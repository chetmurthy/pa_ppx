#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,rresult,fmt,pa_ppx_utils,compiler-libs.common"
version = "$Version::version"
description = "pa_ppx base support"

# For linking
package "link" (
requires = "camlp5,rresult,fmt,pa_ppx_utils,compiler-libs.common"
archive(byte) = "pa_ppx_base.cma"
archive(native) = "pa_ppx_base.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_base.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,rresult,fmt,pa_ppx_utils,compiler-libs.common"
  archive(syntax,preprocessor,-native) = "pa_ppx_base.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_base.cmxa"

EOF
