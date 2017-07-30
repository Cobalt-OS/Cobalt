#!/bin/bash

NAME="Corbin Davenport"
PUBLISHER="Cobalt"
TITLE="Cobalt Live CD"
FILE="COBALT.ISO"
DIR="$(dirname "$0")"

# detect platform and set proper mkisofs binary

cd $DIR
if [ "$(uname)" == "Darwin" ]; then # Mac OS X
	MKISOFS="./mkisofs-mac"
	echo "[ OK ] Platform set to Mac OS X, using $MKISOFS"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then # Linux
	MKISOFS="./mkisofs-linux"
	echo "[ OK ] Platform set to Linux, using $MKISOFS"
else
	MKISOFS="/usr/bin/mkisofs"
	echo "[WARN] Platform could not be detected, trying binary at $MKISOFS"
	echo "If this causes issues, you may need to install mkisofs manually to that directory."
fi

# make sure mkisofs is executable

if ! [ -x "$MKISOFS" ]; then
	chmod +x "$MKISOFS"
	echo "[ OK ] $MKISOFS not marked as executable, fixed"
fi

# check for files

if [ -f "$FILE" ]; then
	rm "$FILE"
	echo "[ OK ] Deleting existing $FILE file."
fi

# compile

echo "[ OK ] Now compiling..."
echo -e "[ OK ] \c"
"$MKISOFS" -quiet -o "$FILE" -p "$NAME" -publisher "$PUBLISHER" -V "$TITLE" -b ISOLINUX/ISOLINUX.BIN -no-emul-boot -boot-load-size 4 -boot-info-table -N -J -r -c boot.catalog -hide boot.catalog -hide-joliet boot.catalog CDROOT
echo "[ OK ] Compile finished"
exit 0
