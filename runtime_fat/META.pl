#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "rresult,fmt,sexplib0"
version = "$Version::version"
description = "pa_ppx runtime support"

# For linking
archive(byte) = "pa_ppx_runtime_fat.cma"
archive(native) = "pa_ppx_runtime_fat.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_runtime_fat.cma"

EOF
