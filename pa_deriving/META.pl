#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_deriving" preprocessor:
version = "$version"
description = "pa_ppx_deriving base support"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_base.link"
archive(byte) = "pa_deriving.cma"
archive(native) = "pa_deriving.cmxa"
)

# For the toploop:
requires(byte,toploop) = "camlp5,fmt,pa_ppx_base"
archive(byte,toploop) = "pa_deriving.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor,-native) = "pa_deriving.cma"
  archive(syntax,preprocessor,native) = "pa_deriving.cmxa"
EOF
