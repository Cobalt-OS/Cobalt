#!/usr/bin/perl
# $Id: bin2c.pl,v 1.5 2003/07/01 00:49:31 hpa Exp $
# -----------------------------------------------------------------------
#   
#   Copyright 1998 H. Peter Anvin - All Rights Reserved
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
#   USA; either version 2 of the License, or (at your option) any later
#   version; incorporated herein by reference.
#
# -----------------------------------------------------------------------
#
# bin2c.pl: binary file to C source converter
#

eval { use bytes; };
eval { binmode STDIN; };

if ( $#ARGV != 0 ) {
    print STDERR "Usage: $0 table_name < input_file > output_file\n";
    exit 1;
}

($table_name) = @ARGV;

printf "unsigned char %s[] = {\n", $table_name;

$pos = 0;
$linelen = 8;

$total_len = 0;

while ( ($n = read(STDIN, $data, 4096)) > 0 ) {
    $total_len += $n;
    for ( $i = 0 ; $i < $n ; $i++ ) {
	$byte = substr($data, $i, 1);
	if ( $pos >= $linelen ) {
	    print ",\n\t";
	    $pos = 0;
	} elsif ( $pos > 0 ) {
	    print ", ";
	} else {
	    print "\t";
	}
	printf("0x%02x", unpack("C", $byte));
	$pos++;
    }
}

printf "\n};\n\nunsigned int %s_len = %u;\n", $table_name, $total_len;

@st = stat STDIN;

printf "\nint %s_mtime = %d;\n", $table_name, $st[9];

exit 0;
