#!/usr/bin/env perl

use strict ;
use IPC::System::Simple qw(systemx runx capturex $EXITVAL);
use String::ShellQuote ;
use File::Basename;

our $version = "0.01" ;

our %pkgmap = (
  'pa_ppx_pa_deriving' => 'pa_ppx.deriving',
  'pa_ppx_pa_deriving_plugins' => 'pa_ppx.deriving_plugins',
  'pa_ppx_pa_base' => 'pa_ppx.base',
  'pa_ppx_pa_import' => 'pa_ppx.import',
  'pa_ppx_pa_unmatched_vala' => 'pa_ppx.unmatched_vala',
  'pa_ppx.hashrecons' => 'pa_ppx.hashrecons',
  
    );

{
  my $basemeta = indent(2, fixdeps(capturex("./base/META.pl"))) ;
  my $uvmeta = indent(2, fixdeps(capturex("./pa_unmatched_vala/META.pl"))) ;
  my $hrmeta = indent(2, fixdeps(capturex("./pa_hashrecons/META.pl"))) ;
  my $dermeta = indent(2, fixdeps(capturex("./pa_deriving/META.pl"))) ;
  my $derpmeta = indent(2, fixdeps(capturex("./pa_deriving.plugins/META.pl"))) ;
  my $impmeta = indent(2, fixdeps(capturex("./pa_import/META.pl"))) ;

  print <<"EOF";
version = "0.01"
description = "pa_ppx: camlp5-based PPX rewriters"

package "base" (
$basemeta
)
package "unmateched_vala" (
$uvmeta
)
package "hashrecons" (
$hrmeta
)
package "deriving" (
$dermeta
)
package "deriving_plugins" (
$derpmeta
)
package "import" (
$impmeta
)

EOF
}

sub fixdeps {
  my $txt = join('', @_) ;
  $txt =~ s,^(.*require.*)$, fix0($1) ,mge;
  return $txt ; 
}

sub fix0 {
  my $txt = shift ;
  $txt =~ s,"([^"]+)", '"'. fix($1) .'"' ,e;
  return $txt ;
}

sub fix {
  my $txt = shift ;
  my @l = split(/,/,$txt);
  my @ol = () ;
  foreach my $p (@l) {
    push(@ol, ($pkgmap{$p} || $p));
  }
  $txt = join(',', @ol) ;
  return $txt ;
}

sub indent {
  my $n = shift ;
  my $txt = shift ;
  my $pfx = ' ' x $n ;
  $txt =~ s,^,$pfx,gm;
  return $txt ;
}
