#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,fmt,pa_ppx_base"
version = "$version"
description = "pa_ppx pa_expect_test support"

# For linking
archive(byte) = "pa_ppx_expect_test.cma"
archive(native) = "pa_ppx_expect_test.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_expect_test.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor) = "pa_ppx_expect_test.cma"
)

EOF
