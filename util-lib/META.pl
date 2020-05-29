#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Base libraries for "pa_ppx"
requires = "pcre,core_kernel"
version = "$version"
description = "pa_ppx base libraries"

archive(byte) = "pa_ppx_utils.cma"
archive(native) = "pa_ppx_utils.cmxa"

EOF
