#!/bin/sh

set -e

BIN=$1
OL=owl-lisp/bin/ol

fail() {
   echo "Fail: $@"
   exit 1
}

chequal() {
   diff $1 $2 || fail $3
}
test -x $BIN || fail "Cannot execute '$BIN'"

mkdir -p tmp

for MAX in 16 32 64 128 256 512 1024 2048 4096
do

   echo "Test round, max $MAX"
   echo " - generating data"
   blab/bin/blab -o tmp/out-%n -n 10 -e "([ab]{1,8}'\\n'){10,$MAX}"
   NL=$(cat tmp/out-* | wc -l)
   NB=$(cat tmp/out-* | wc -c)

   echo " - one file"
   $BIN --sort tmp/out-1 > tmp/x
   cat tmp/out-1 | sort | uniq > tmp/y
   chequal tmp/[xy] "single file is not like sort | uniq"

   echo " - self diff"
   $BIN --difference tmp/out-1 tmp/out-1 > tmp/x
   test -s tmp/x && fail "difference with self is not empty"

   echo " - self blacklist"
   $BIN --blacklist tmp/out-1 tmp/out-1 > tmp/x
   test -s tmp/x && fail "blacklisting self does not cause null"

   echo " - unions"
   $BIN --sort --union tmp/out-* > tmp/all-1
   cat tmp/out-* | sort | uniq > tmp/all-2
   chequal tmp/all-1 tmp/all-2 U1

   echo " - intersection + reverse intersection"
   $BIN --intersect tmp/out-[123] > tmp/x
   $BIN --reverse-intersect tmp/out-[123] > tmp/y
   $BIN --sort tmp/out-[123] > tmp/all123-1
   $BIN --sort --union tmp/x tmp/y > tmp/all123-2
   chequal tmp/all123-1 tmp/all123-2 "intersection + reverse intersection does not give full set"
   $BIN --intersect tmp/x tmp/y > tmp/x
   test -s tmp/x && fail "intersection of normal and reverse intersection is non-empty"

   echo " - blacklist vs difference"
   $BIN --blacklist tmp/out-1 --blacklist tmp/out-2 tmp/out-[34] > tmp/x
   $BIN --union tmp/out-[34] | $BIN --difference - tmp/out-[12] > tmp/y
   chequal tmp/[xy]

   echo " - differences and intersections"
   $BIN --sort --difference tmp/out-1 tmp/out-2 > tmp/diff-1
   $BIN --intersect tmp/out-1 tmp/out-2 | $BIN --sort --difference tmp/out-1 - > tmp/diff-2
   diff tmp/diff-1 tmp/diff-2 || fail diffs

   echo " - cover"
   $BIN --sort --union $($BIN --cover tmp/out-* tmp/diff-*) > tmp/all-3
   diff tmp/all-1 tmp/all-3 || fail cover

   echo " - union + sort"
   START=$($OL -e '(time-ms)')
   $BIN --sort tmp/out-* > tmp/all-1
   LINESMS=$($OL -e "(- (time-ms) $START)")
   START=$($OL -e '(time-ms)')
   cat tmp/out-* | sort | uniq > tmp/all-2
   TOOLMS=$($OL -e "(- (time-ms) $START)")
   diff tmp/all-1 tmp/all-2 || fail "tool outputs differ for union + sort"
   echo "   + ${NB}b/${NL}l: ${LINESMS}ms, sort | uniq ${TOOLMS}ms"

done

echo "Everything seems to be in order"
