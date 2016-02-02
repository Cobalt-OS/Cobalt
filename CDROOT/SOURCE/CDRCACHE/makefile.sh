#!/bin/bash

# old NASM versions need -d, newer ones -D ...
# Options mean:
# DEBUGNB to see when cache becomes full
# REDIRBUG if redirection of messages does not work on old FreeDOS kernels
# SANE_A20 to suppress waiting for keyboard controller ready before XMS
#   copy - the waiting works around a Bochs 1.x BIOS bug.

echo Now creating cdrcache.sys and cdrcache.com...

if true; then
  echo Creating normal cdrcache...
  nasm -DSANE_A20=1 -o cdrcache.sys cdrcache.asm
else
  echo Creating DEBUG version of cdrcache, including workarounds...
  nasm -DSDEBUGNB=1 -DREDIRBUG=1 -o cdrcachd.sys cdrcache.asm
fi

upx --8086 cdrcach*.sys
echo 'cdrcach*.sys has been updated.'

ls -l *.sys

echo 
echo Please move cdrcach*.sys to ../../BIN/
echo if you want to make them the active version now.

