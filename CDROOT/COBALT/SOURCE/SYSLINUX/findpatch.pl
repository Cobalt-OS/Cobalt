#!/usr/bin/perl
#
# Script to find the "patch area" of ldlinux.sys
#

eval { use bytes; };

open(SYS, "< ldlinux.sys") or die "$0: Cannot open ldlinux.sys\n";
eval { binmode SYS; };
if ( read(SYS,$sec1,512) != 512 ) {
    die "$0: ldlinux.sys: short read\n";
}
close(SYS);

for ( $i = 0 ; $i < 512; $i++ ) {
    $scan = substr($sec1,$i,12);

    if ( $scan eq "\032LDLINUX SYS" &&
	 substr($sec1,$i+16,2) eq "\x55\xAA" ) {
	last;
    }
}


die "$0: Did not find patch area signature\n" unless ( $i < 512 );

# Past signature, plus align to the subsequent dword.
print ((($i+18)+3) & ~3); print "\n";
