#!/bin/bash

NAME="Corbin Davenport"
PUBLISHER="Cobalt"
TITLE="Cobalt Live CD"
FILE="cobalt.iso"
DIR="$(dirname "$0")"

# make sure mkisofs is executable
function fix_permissions {
	if ! [ -x "$MKISOFS" ]; then
		chmod +x "$MKISOFS"
		echo "[ OK ] $MKISOFS not marked as executable, fixed"
	fi
}

# detect platform and set proper mkisofs binary

cd $DIR
if [ -x "$(command -v mkisofs)" ]; then # if mkisofs is already installed
	MKISOFS="mkisofs"
	echo "[ OK ] Mkisofs binary already installed, the included binary will not be used"
elif [ -x "$(command -v genisoimage)" ]; then # debian uses a forked version of mkisofs called genisoimage
	MKISOFS="genisoimage"
	echo "[ OK ] Genisoimage binary already installed, the included binary will not be used"
elif [ "$(uname)" == "Darwin" ]; then # macOS
	MKISOFS="./mkisofs-mac"
	echo "[ OK ] Mkisofs binary not installed, using $MKISOFS instead"
	fix_permissions
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then # linux
	MKISOFS="./mkisofs-linux"
	echo "[ OK ] Mkisofs binary not installed, using $MKISOFS instead"
	fix_permissions
else
	MKISOFS="/usr/bin/mkisofs"
	echo "[WARN] Platform could not be detected, trying binary at $MKISOFS"
	echo "If this causes issues, you may need to install mkisofs manually to that directory"
fi

# check for zip command
if [ -x "$(command -v zip)" ]; then
	echo "[ OK ] ZIP command found"
else
	echo "[EROR] ZIP command could not be found. Please install zip first"
	exit 0
fi

# compile BASE.ZIP file
if [ -f CDROOT/COBALT/BASE.ZIP ]; then
	rm CDROOT/COBALT/BASE.ZIP
	echo "[ OK ] Deleted existing BASE.ZIP"
fi
cd packages/base
zip -q -r ../../CDROOT/COBALT/BASE.ZIP *
cd ../../
echo "[ OK ] Finished compiling BASE.ZIP"

# compile DESKTOP.ZIP file
if [ -f CDROOT/COBALT/DESKTOP.ZIP ]; then
	rm CDROOT/COBALT/DESKTOP.ZIP
	echo "[ OK ] Deleted existing DESKTOP.ZIP"
fi
cd packages/desktop
zip -q -r ../../CDROOT/COBALT/DESKTOP.ZIP *
cd ../../
echo "[ OK ] Finished compiling DESKTOP.ZIP file"

# compile VBOX.ZIP file
if [ -f CDROOT/COBALT/VBOX.ZIP ]; then
	rm CDROOT/COBALT/VBOX.ZIP
	echo "[ OK ] Deleted existing VBOX.ZIP"
fi
cd packages/virtualbox
zip -q -r ../../CDROOT/COBALT/VBOX.ZIP *
cd ../../
echo "[ OK ] Finished compiling VBOX.ZIP file"

# check for ISO file
if [ -f "$FILE" ]; then
	rm "$FILE"
	echo "[ OK ] Deleting existing $FILE file"
fi

# compile

echo "[ OK ] Now compiling $FILE..."
echo -e "[ OK ] \c"
"$MKISOFS" -quiet -o "$FILE" -p "$NAME" -publisher "$PUBLISHER" -V "$TITLE" -b ISOLINUX/ISOLINUX.BIN -no-emul-boot -boot-load-size 4 -boot-info-table -N -J -r -c boot.catalog -hide boot.catalog -hide-joliet boot.catalog CDROOT
echo "[ OK ] Compile finished"
exit 0
