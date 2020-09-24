#!/usr/bin/env perl

use strict;
use Data::Dumper ;

{
  my $kind = $ARGV[0] ;
  my $oroot = $ARGV[1] ;
  my $oversion = $ARGV[2] ;
  die "mal-formatted $kind version detected: please report to maintainer with this output: ".Dumper(\@ARGV)
    unless $oversion =~ m,^(?:\d+(?:\.\d+)+)(?:[\+-][a-z]+\d+)?$, ;

  if (-d "generated_src/$oversion") {
    print "$oversion\n";
    exit ;
  }

  print STDERR "WARNING: missing directory generated_src/$oversion\n" if (! -d "generated_src/$oversion") ;

  my @versions = <generated_src/${oroot}*> ;
  @versions = sort @versions ;
  $oversion = $versions[-1] ;
  $oversion =~ s,.*/,,;
  if (-d "generated_src/$oversion") {
    print STDERR "WARNING: FALLING BACK to saved info for $kind version $oversion; please report to maintainer\n" ;
    print "$oversion\n";
    exit ;
  }


  # let the configure script fail in the usual way
  print "$oversion\n";
}
