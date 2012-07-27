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

      # don't register disallowed modules
      # as dependencies (e.g. for system wide modules).
      foreach my $mod (@skipmods) {
        if ($mod eq $modname) { $skipmod = 1;}
      }
      next if ($skipmod);

      # check if the module is in local directory
      my $newfile = $modname . '.f90';
      if (-f $newfile) {
        push @uselist,  $newfile;
        $donemod = 1;
      } else {
        $newfile = $modname . '.F90';
        if (-f $newfile) {
          push @uselist,  $newfile;
          $donemod = 1;
        }
      }
      next if ($donemod);

      # check if the module is in an include directory
      foreach my $dir (@searchdirs) {
        $dir =~ s/\/$//;
        my $newfile = $dir . '/' . $modname . '.f90';
        if (-f $newfile) {
          push @uselist,  $newfile;
          $donemod = 1;
        } else {
          $newfile = $dir . '/' . $modname . '.F90';
          if (-f $newfile) {
            push @uselist,  $newfile;
          $donemod = 1;
          }
        }
        last if ($donemod);
      }

      if (! $donemod) {
        print STDERR "WARNING: no match found for module '$modname' in file '$file'\n";
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

  # simple version. depend on object files only
  if (0) {
    print DEP "$obj : $file";
    foreach $obj (@uselist) {
      $obj =~ s/[fF]90$/o/;
      print DEP " $obj";
    }
    print DEP "\n";
  } else {
    # use dependencies on .mod files. may require
    # much less recompiles if .mod files are not
    # changed, even if the source changes (as it
    # happens with gfortran.
    print DEP "$obj : $file";
    foreach $obj (@uselist) {
      $obj =~ s/[fF]90$/mod/;
      print DEP " $obj";
    }
    print DEP "\n";

  }
}
close DEP;
rename $out . '.tmp', $out;
