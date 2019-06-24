#!/usr/bin/env perl

use strict;
use warnings;
use v5.26;
use Data::Dumper;

my $script = $ARGV[0] // './bots/nearest_with_manips.py';
opendir my $dir, "./data/" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

my @problems = grep { $_ =~ /prob-\d\d\d\.desc/ } @files;

for my $problem (sort @problems) {
  my $size = -s "data/$problem";
  warn $size;
#  next if $size <= 10000;
  my $solution = $problem;
  $solution =~ s/desc/sol/;
  say "Working on $problem";
  system("$script $problem > solutions/tmp/$solution");

  if (! -e "solutions/$solution" || -s "solutions/tmp/$solution" < -s "solutions/$solution") {
    warn "better solution by " . (-s "solutions/$solution" - -s "solutions/tmp/$solution");
    system("mv solutions/tmp/$solution solutions/$solution");
  }
}
