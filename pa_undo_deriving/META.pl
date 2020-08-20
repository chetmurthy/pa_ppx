#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_undo_deriving" preprocessor:
requires = "pa_ppx_base,fmt,bos,compiler-libs.common"
version = "$Version::version"
description = "pa_ppx_undo_deriving base support"

# For linking
package "link" (
requires = "pa_ppx_base.link,fmt,bos,compiler-libs.common"
archive(byte) = "pa_undo_deriving.cma"
archive(native) = "pa_undo_deriving.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_undo_deriving.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pa_ppx_base,fmt,bos,compiler-libs.common"
  archive(syntax,preprocessor,-native) = "pa_undo_deriving.cma"
  archive(syntax,preprocessor,native) = "pa_undo_deriving.cmxa"

EOF
