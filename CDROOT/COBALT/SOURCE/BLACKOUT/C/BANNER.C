//
//	Banner 1.1 - (C) 1997 Joseph P Morris
//
//	Do what you want with it.. just remember that I wrote it originally.
//

//
//	Compile this in TINY model, and use exe2bin
//

#include <stdio.h>
#include <dos.h>
#include <mem.h>

// Variables (global to save space by sharing them)

char pal[768];
struct REGPACK r;
int ctr;
char buf[320];
unsigned int scr=0;

// Palette Set

void ps()
{
r.r_ax=0x1012;
r.r_bx=0;
r.r_cx=256;
r.r_es=FP_SEG(pal);
r.r_dx=FP_OFF(pal);
intr(0x10,&r);
}

// Raw Read screen (as opposed to PCX(); )

void RAW()
{
FILE *fp;

// open file

fp=fopen("c:\\openinit.raw","rb");
if(!fp)
	return;

// Read in the palette

fread(pal,768,1,fp);

// Read in the screen line-by-line (TINY model doesn't allow far reads)

for(ctr=0;ctr<199;ctr++)
	{
	fread(buf,320,1,fp);
	movedata(_DS,FP_OFF(buf),0xa000,scr+=320,320);
	}

// close file

fclose(fp);
}

// Main function

main()
{
memset(pal,768,0);	// Blank palette

// enter mode 13h

_AX=0x13;
geninterrupt(0x10);

// set palette to black

ps();

// Crush the font to oblivion

r.r_ax=0x1121;
r.r_bx=0;
r.r_cx=1;
r.r_es=FP_SEG(pal);
r.r_bp=FP_OFF(pal);
r.r_dx=1;
intr(0x10,&r);

// Raw Read the screen

RAW();

// Set the new palette

ps();
}