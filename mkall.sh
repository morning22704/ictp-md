#!/bin/sh
for d in build-*
do \
    make -C $d clean
    make -C $d || exit 1
done
