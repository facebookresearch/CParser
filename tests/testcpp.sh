#!/bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#
# This script compares the outputs of the clang preprocessor
# and the cparser preprocessor. Other than white-space
# differences, we expect parity on MacOSX 10.10.
# Note that we have to prevent clang from including
# the compiler specific include files because
# they contain weird constructs such as #include_next
# and friends. I do not want to implement that.


tmp=tmp$$-

trap "rm 2>/dev/null ${tmp}*" 0

usage() {
    echo 2>&1 "Usage: cppcomp.sh -I... -D... filename.c"
}

args=
file=
defs='"-Znopass"'
for n in "$@"
do
    case "$n" in
	-Z*)
	    defs="$defs,\"$n\""
	    ;;
	-*)
	    args="$args $n"
	    defs="$defs,\"$n\""
	    ;;
	*)
	    test -n "$file" && usage && exit 10
	    file=$n
    esac
done
test -z "$file" && usage && exit 10

defs="$defs,"'"-I.","-I-","-I/usr/include","-Zcppdef"'
defs="$defs,"'"-D__has_builtin(x)=1","-D__has_feature(x)=1"'
defs="$defs,"'"-D__has_attribute(x)=1","-D__has_extension(x)=1"'

clang -E -nostdinc $args -I. -I/usr/include $file > ${tmp}-clang.c
lua -lcparser -e 'cparser.cpp("'$file'", "'${tmp}-lua.c'", {'$defs'})'
grep -v '^[[:space:]]*#' ${tmp}-clang.c | grep -v '^[[:space:]]*$' > ${tmp}-clang2.c
grep -v '^[[:space:]]*#' ${tmp}-lua.c | grep -v '^[[:space:]]*$' > ${tmp}-lua2.c
wc ${tmp}-*

diff -u -b -B ${tmp}-clang2.c ${tmp}-lua2.c
