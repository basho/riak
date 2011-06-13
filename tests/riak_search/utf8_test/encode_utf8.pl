#!/usr/bin/perl

# This script converts UTF8 text to ampersand encoded text, ready for XML.

use strict;
use warnings;

for my $file ( @ARGV ){
  open my $fh, '<:utf8', $file or die "cannot open file $file: $!";
  while( <$fh> ){
    s/([\x7f-\x{ffffff}])/'&#'.ord($1).';'/ge;
    print;
  }
}
