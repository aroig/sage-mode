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
(grep sage-mode-version emacs/sage.el | grep "$VERSION" >/dev/null 2>/dev/null) \
    || (echo; echo "WARNING: version number in sage.el doesn't match $VERSION!"; echo)

${EMACS-emacs} --batch -Q --load emacs/sage.el --funcall 'sage-update-autoloads'
rm emacs/sage-load.el~

popd

# Create spkg
sage --pkg "$SPKG_DIR"
