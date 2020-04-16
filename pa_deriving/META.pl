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
archive(byte) = "pa_deriving.cma"
archive(native) = "pa_deriving.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_deriving.cma"

# For the preprocessor itself:
archive(syntax,preprocessor) = "pa_deriving.cma"

EOF
