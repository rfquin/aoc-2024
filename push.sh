#!/usr/bin/env sh

if [ $# -eq 0 ]; then
    echo "Must provide a argument for the commit message name"
    exit 1
fi

git add *
git commit -am "$1"
git push
