#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "rresult,fmt,ppx_deriving_protobuf.runtime"
version = "$version"
description = "pa_ppx protobuf runtime support"

# For linking
archive(byte) = "pa_ppx_protobuf.cma"
archive(native) = "pa_ppx_protobuf.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_protobuf.cma"

EOF
