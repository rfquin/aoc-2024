#!/usr/bin/env sh

if [ $# -eq 0 ]; then
    exit 1
fi

git add *
git commit -am "$1"
git push
