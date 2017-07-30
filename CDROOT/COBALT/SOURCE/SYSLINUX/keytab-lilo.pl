#!/usr/bin/perl
# --------------------------------------------------------------------------
# This program was taken from the LILO-20 distribution; only this header
# was added.
#
# LILO program code, documentation and auxiliary programs are
# Copyright 1992-1997 Werner Almesberger.
# All rights reserved.
#
# Redistribution and use in source and binary forms of parts of or the
# whole original or derived work are permitted provided that the
# original work is properly attributed to the author. The name of the
# author may not be used to endorse or promote products derived from
# this software without specific prior written permission. This work
# is provided "as is" and without any express or implied warranties.
# --------------------------------------------------------------------------

eval { use bytes; }; eval { binmode STDOUT; };

$DEFAULT_PATH = "/usr/lib/kbd/keytables";
$DEFAULT_MAP = "us";
$DEFAULT_EXT = ".map";

sub usage
{
    print STDERR
      "usage: $0 [ -p old_code=new_code ] ...\n".
      (" "x(8+length $0))."[path]default_layout[.map] ] ".
      "[path]kbd_layout[.map]\n";
    exit 1;
}


while ($ARGV[0] eq "-p") {
    shift(@ARGV);
    &usage unless $ARGV[0] =~ /=/;
    $table[eval($`)] = eval($');
    shift(@ARGV);
}
&usage unless defined $ARGV[0];
load_map("def",defined $ARGV[1] ? $ARGV[0] : undef);
load_map("kbd",defined $ARGV[1] ? $ARGV[1] : $ARGV[0]);
&build_table("plain","shift","ctrl","altgr","shift_ctrl",
  "altgr_ctrl","alt","shift_alt","ctrl_alt");
for ($i = 0; $i < 256; $i++) {
    printf("%c",$table[$i] ? $table[$i] : $i) || die "print: $!";
}
close STDOUT || die "close: $!";


sub load_map
{
    local ($pfx,$map) = @_;
    local ($empty,$current);

    $map = $DEFAULT_MAP unless defined $map;
    $map = $DEFAULT_PATH."/".$map unless $map =~ m|/|;
    $map .= $DEFAULT_EXT unless $map =~ m|/[^/]+\.[^/]+$|;
    if (!open(FILE,"loadkeys -m $map |")) {
	print STDERR "loadkeys -m $map: $!\n";
	exit 1;
    }
    undef $current;
    $empty = 1;
    while (<FILE>) {
	chop;
	if (/^u_short\s+(\S+)_map\[\S+\]\s+=\s+{\s*$/) {
	    die "active at beginning of map" if defined $current;
	    $current = $pfx.":".$1;
	    next;
	}
	undef $current if /^};\s*$/;
	next unless defined $current;
	s/\s//g;
	$map{$current} .= $_;
	$empty = 0;
    }
    close FILE;
    return unless $empty;
    print STDERR "Keymap is empty\n";
    exit 1;
}


sub build_table
{
    local (@maps) = @_;
    local (@tmp);

    $set = 0;
    for $map (@maps) {
	$code = $set;
	for (split(",",$map{"def:".$map})) {
	    die "bad map entry $_ (def, map $map)" unless /^0x\S\S(\S\S)$/;
	    $tmp[$code] = hex $1 unless $tmp[$code];
	    $code++;
	}
	$set += 256;
    }
    $set = 0;
    for $map (@maps) {
	$code = $set;
	for (split(",",$map{"kbd:".$map})) {
	    die "bad map entry $_ (kbd, map $map)" unless /^0x\S\S(\S\S)$/;
	    $table[$tmp[$code]] = hex $1 unless $table[$tmp[$code]];
	    $code++;
	}
	$set += 256;
    }
    $table[0] = 0;
}
