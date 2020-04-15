#!/usr/bin/env perl

use strict ;

our $version = "0.01" ;
our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5"
version = "$version"
description = "pa_ppx base support"
directory = "$destdir/pa_ppx_base"

# For linking
archive(byte) = "ppxutil.cmo pa_passthru.cmo pa_unmatched_vala.cmo"
archive(native) = "ppxutil.cmx pa_passthru.cmx pa_unmatched_vala.cmx"

# For the toploop:
archive(byte,toploop) = "ppxutil.cmo pa_passthru.cmo pa_unmatched_vala.cmo"

# For the preprocessor itself:
archive(syntax,preprocessor) = "ppxutil.cmo pa_passthru.cmo pa_unmatched_vala.cmo"

EOF
