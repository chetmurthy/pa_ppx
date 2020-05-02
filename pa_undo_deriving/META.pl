#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_undo_deriving" preprocessor:
requires = "pa_ppx_base,fmt,bos,compiler-libs.common"
version = "$version"
description = "pa_ppx_undo_deriving base support"

# For linking
archive(byte) = "pa_undo_deriving.cma"
archive(native) = "pa_undo_deriving.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_undo_deriving.cma"

package "syntax" (
  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pa_ppx_base,fmt,bos,compiler-libs.common"
  archive(syntax,preprocessor) = "pa_undo_deriving.cma"
)

EOF
