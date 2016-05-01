#!/usr/bin/perl
#
# Read the "version" file and produce some macro declarations
#

use Fcntl;

$vfile = $ARGV[0];
sysopen(VERSION, $vfile, O_RDONLY) or die "$0: Cannot open $vfile\n";
$version = <VERSION>;
chomp $version;
close(VERSION);

unless ( $version =~ /^([0-9]+)\.([0-9]+)$/ ) {
    die "$0: Cannot parse version format\n";
}
$vma = $1+0; $vmi = $2+0;

open(VI, "> version.gen") or die "$0: Cannot create version.gen\n";
print VI "%define VERSION \"$version\"\n";
print VI "%define VER_MAJOR $vma\n";
print VI "%define VER_MINOR $vmi\n";
close(VI);


