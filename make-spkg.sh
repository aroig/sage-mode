#!/bin/bash

set -e

VERSION="$1"

if [ -z "$VERSION" ]; then
    echo "usage: $0 VERSION"
    exit 1
fi

# Clean out the directory
SPKG_DIR=./sage_mode-"$VERSION"
rm -rf "$SPKG_DIR"
mkdir "$SPKG_DIR"
pushd "$SPKG_DIR"

# Clone
hg clone .. .

# Create autoloads
emacs --batch -Q --load emacs/sage.el --funcall 'sage-update-autoloads'
rm emacs/sage-load.el~

popd

# Create spkg
sage --pkg "$SPKG_DIR"
