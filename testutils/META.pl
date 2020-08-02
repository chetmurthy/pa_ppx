#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# test utilities for "pa_ppx"
requires = "unix,camlp5,ounit2,fmt,pcre,rresult,compiler-libs.common,yojson,sexplib"
version = "$version"
description = "pa_ppx test utilities"

archive(byte) = "pa_ppx_testutils.cma"
archive(native) = "pa_ppx_testutils.cmxa"

EOF
