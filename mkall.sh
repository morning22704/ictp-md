#!/bin/sh

# create build dirs as needed
for s in `cd config; ls *-*; cd ..`
do \
   test -d build-$s || mkdir build-$s
   test -e build-$s/Makefile || ln -s ../config/$s build-$s/Makefile
done

# (re)build executables
for d in build-*
do \
    make -C $d clean
    make -C $d || exit 1
    make -C $d check || exit 1
done

# build the manual
make -C $d doc
