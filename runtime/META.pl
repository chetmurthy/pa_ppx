#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "rresult,fmt"
version = "$version"
description = "pa_ppx runtime support"

# For linking
archive(byte) = "pa_ppx_runtime.cma"
archive(native) = "pa_ppx_runtime.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_runtime.cma"

EOF
