#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Base libraries for "pa_ppx"
requires = "pcre,core_kernel"
version = "$version"
description = "pa_ppx base libraries"

# For linking
archive(byte) = "pa_ppx_utils.cma"
archive(native) = "pa_ppx_utils.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_utils.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pcre,core_kernel"
  archive(syntax,preprocessor) = "pa_ppx_utils.cma"
)

EOF
