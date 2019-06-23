#!/usr/bin/env perl

use strict;
use warnings;
use v5.26;
use Data::Dumper;

my $script = './bots/nearest_space.py';
opendir my $dir, "./data/" or die "Cannot open directory: $!";
my @files = readdir $dir;
closedir $dir;

my @problems = grep { $_ =~ /prob-\d\d\d\.desc/ } @files;

for my $problem (@problems) {
  my $size = -s "data/$problem";
  warn $size;
  next if $size > 10000;
  my $solution = $problem;
  $solution =~ s/desc/sol/;
  say "Working on $problem";
  system("$script $problem > solutions/tmp/$solution");

  if (-s "solutions/tmp/$solution" < -s "solutions/$solution") {
    system("mv solutions/tmp/$solution solutions/$solution");
  }
}
