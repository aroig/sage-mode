#!/bin/sh

if [ "x$1" = "x" -o "x$2" = "x" ]; then
    echo usage: $0 EMACSCLIENT IMAGE_FILE
    exit 1
fi
"$1" -e '(sage-view-handle-emacslient "'"$2"'")'
