#!/usr/bin/perl
# simple custom regression test wrangler
# FIXME: add some inline docs here
# (c) 2012 Axel Kohlmeyer <akohlmey@gmail.com>
use strict;

# declare and initialize variables
my ($cmd,$cmdline,$dir,$sec,$num,$regexp,$line,$pass,$fail,$status,$verbose);
$line='';
$verbose=0;
$pass=0;
$fail=0;

die "Too few arguments: @ARGV\n"
  unless ($#ARGV > 0);

$dir = pop(@ARGV);
$cmd = join(' ',@ARGV);
print STDOUT "--------------------------------\n";
print STDOUT "Test command: $cmd\n";
print STDOUT "--------------------------------\n";

# verify if the test suite is present
die "Cannot find test suite directory '$dir'\n"
  unless -d $dir;
open SECTS, "< $dir/sections"
  or die "Could not open test sections list in directory $dir: $!\n";

# loop over test sections
while (<SECTS>) {
  next if (/^\s*\#.*/);
  next if (/^\s*$/);
  chomp;
  $sec = $_;
  print STDERR "Processing test section: $sec\n" if ($verbose);

  # verify if this section has tests listed
  die "Cannot find test section '$sec'\n"
    unless -d "$dir/$sec";
  open LIST, "< $dir/$sec/list"
    or die "Could not open test list: $!\n";

  # loop over individual tests in section
  while (<LIST>) {
    chomp;
    next if (/^\s*\#.*/);
    next if (/^\s*$/);
    $num = $_;
    print STDERR "Processing test $sec/$num\n" if ($verbose);

    # validate test data
    die "Test input for test $sec/$num does not exist\n"
      unless -f "$dir/$sec/test_$num.in";
    die "Test reference for test $sec/$num does not exist\n"
      unless -f "$dir/$sec/test_$num.ref";

    # assemble command line and execute
    $cmdline = $cmd . " < $dir/$sec/test_$num.in";
    $cmdline .= " > $sec-$num.out 2> $sec-$num.err";
    print STDERR "Executing command: '$cmdline'\n" if ($verbose);
    system($cmdline);

    # open reference data and command output
    open REGEXP, "< $dir/$sec/test_$num.ref"
      or die "Could not open reference file: $!\n";
    open OUT, "< $sec-$num.out"
      or die "Could not open test run output: $!\n";

    # loop over regular expressions until we find 'END-OF-RUN'
    # and read from command output until a regular expression
    # matches and then move on to the next expression.
    $status = 0;
    while (<REGEXP>) {
      chomp;
      next if (/^\s*\#.*/);
      next if (/^\s*$/);
      last if (/^END-OF-RUN$/);
      $regexp = $_;
      print STDERR "Testing against expression '$regexp'\n"  if ($verbose);

      while (1) {
        $line = <OUT> or $status = 1;
        last if ($status);

        chomp $line;
        print STDERR "line to test: $line\n" if ($verbose);
        last if ($line =~ /$regexp/);
      }
      last if ($status);
    }
    close REGEXP;
    close OUT;

    # FIXME: compare some generated files, if needed

    if ($status) {  
      print STDOUT "Test $sec/$num FAILED\n";
      ++ $fail;
    } else {
      print STDOUT "Test $sec/$num PASSED\n";
      # remove output, if the test was successful
      unlink ("$sec-$num.out","$sec-$num.err");
      ++ $pass;
    }
  }
  close LIST;
}
close SECTS;

print STDOUT "---------------------------------------------\n";
print STDOUT "Total tests run: ", $fail+$pass;
print STDOUT " | passed: ", $pass;
print STDOUT " | failed: ", $fail, "\n";
print STDOUT "---------------------------------------------\n";

exit 1 if ($fail > 0);
exit 0;
