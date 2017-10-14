#!/bin/sh -ue

REPODIR=$(git rev-parse --show-toplevel)
RFILES="$REPODIR"/R

for i in "$RFILES"/pipe.R "$RFILES"/tidy-ext-tracks.R "$RFILES"/hurricane.R "$RFILES"/wind-rose.R "$RFILES"/geom-hurricane.R
do
    cat $i
    echo
done > "$REPODIR"/tools/submission.R
