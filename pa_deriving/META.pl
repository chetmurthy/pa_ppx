#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx_pa_deriving" preprocessor:
requires = "camlp5,pa_ppx_base"
version = "$version"
description = "pa_ppx_pa_deriving base support"
directory = "$destdir/pa_ppx_pa_deriving"

# For linking
archive(byte) = "pa_deriving.cmo"
archive(native) = "pa_deriving.cmx"

# For the toploop:
archive(byte,toploop) = "pa_deriving.cmo"

# For the preprocessor itself:
archive(syntax,preprocessor) = "pa_deriving.cmo"

EOF
