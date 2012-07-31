#!/usr/bin/perl -w
# custom fortran 90 dependency generator
# (c) 2012 Axel Kohlmeyer <akohlmey@gmail.com>
use strict;
use Getopt::Std;

# define global variables and defaults
my @uselist = ();
my %modlist;
my @skipmods = ();
my @searchdirs = ();

# parse options for include dirs and output file
my %opts;
getopt('o:I:', \%opts);

# store include directories into @searchdirs
if (exists($opts{I})) {
  foreach my $d (split /:/, $opts{I}) {
    push @searchdirs, $d;
  }
}

# set output name
my $out = '.depend';
if (exists($opts{o})) {
  $out = $opts{o};
}

# loop over all remaining arguments
# which are expected to be file names
# of fortran 90 sources.
foreach my $file (@ARGV) {
  @uselist = ();
  open FP, "<$file";

  # read through file. this is no real
  # fortran parser and can be easily fooled. 
  while (<FP>) {

    # skip over comments.
    next if (/\s*!.*/);

    # handle module statements
    if (/^\s*module\s+([a-zA-Z0-9_]+)\s*.*/) {
      my ($modname,$skipmod,$donemod) = ($1,0,0);

      # convert to downcase
      $modname =~ tr/[A-Z]/[a-z]/;

      next if ($modname eq 'procedure');

      # don't register disallowed modules
      # as dependencies (e.g. for system wide modules).
      foreach my $mod (@skipmods) {
        if ($mod eq $modname) { $skipmod = 1;}
      }
      next if ($skipmod);

      print STDERR "WARNING: module $modname already defined in file '$modlist{$modname}'\n" if (exists $modlist{$modname});
      $modlist{$modname} = $file;
    }
  }
  close FP;
}

# we write to a temporary file and
# move it over the real file only
# after a completely successful run.
open DEP , '> ' . $out . '.tmp';

# loop over all remaining arguments
# which are expected to be file names
# of fortran 90 sources.
foreach my $file (@ARGV) {
  @uselist = ();
  open FP, "<$file";

  # read through file. this is no real
  # fortran parser and can be easily fooled. 
  while (<FP>) {

    # skip over comments.
    next if (/\s*!.*/);

    # handle use statements
    if (/^\s*use\s+([a-zA-Z0-9_]+)\s*.*/) {
      my ($modname,$skipmod,$donemod) = ($1,0,0);

      # convert to downcase
      $modname =~ tr/[A-Z]/[a-z]/;

      # don't register disallowed modules
      # as dependencies (e.g. for system wide modules).
      foreach my $mod (@skipmods) {
        if ($mod eq $modname) { $skipmod = 1;}
      }
      next if ($skipmod);

      if (exists $modlist{$modname}) {
        push @uselist, $modlist{$modname};
      } else {
        print STDERR "WARNING: no match found for module '$modname' used in file '$file'\n";
      }
    }
  }
  close FP;

  # read of file completed.
  # now generate Makefile text.

  my $obj = $file;
  $obj =~ s/[fF]90$/o/;
  # chop off any leading path for correct object
  # names when building outside the source dir
  $obj =~ s/^.*\///;

  # we don't include module depencies, use object files only
  print DEP "$obj : $file";
  foreach $obj (@uselist) {
    $obj =~ s/[fF](90|)$/o/;
    # chop off any leading path for correct object
    # names when building outside the source dir
    $obj =~ s/^.*\///;
    print DEP " $obj";
  }
  print DEP "\n";
}
close DEP;
rename $out . '.tmp', $out;
