#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,fmt,bos,pa_ppx_base,pa_ppx_utils"
version = "$version"
description = "pa_ppx pa_dock support"

# For linking
package "link" (
requires = "camlp5,fmt,bos,pa_ppx_base.link,pa_ppx_utils"
archive(byte) = "pa_ppx_dock.cma"
archive(native) = "pa_ppx_dock.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_dock.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,bos,pa_ppx_base,pa_ppx_utils"
  archive(syntax,preprocessor,-native) = "pa_ppx_dock.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_dock.cmxa"

EOF
