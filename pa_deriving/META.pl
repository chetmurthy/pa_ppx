#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_deriving" preprocessor:
version = "$Version::version"
description = "pa_ppx_deriving base support"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_base.link"
archive(byte) = "pa_deriving.cma"
archive(native) = "pa_deriving.cmxa"
)
requires = "camlp5,fmt,pa_ppx_runtime"

# For the toploop:
requires(byte,toploop) = "camlp5,fmt,pa_ppx_base,pa_ppx_runtime"
archive(byte,toploop) = "pa_deriving.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor,-native) = "pa_deriving.cma"
  archive(syntax,preprocessor,native) = "pa_deriving.cmxa"
EOF
