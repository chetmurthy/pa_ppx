#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Base libraries for "pa_ppx"
requires = "pcre,unix,base"
version = "$Version::version"
description = "pa_ppx base libraries"

archive(byte) = "pa_ppx_utils.cma"
archive(native) = "pa_ppx_utils.cmxa"

EOF
