#!/bin/bash
#

set -e

for f in $@; do
  ffprobe -v quiet -print_format json -show_format "$f" |\
    jq -r '[.format.filename,
         .format.tags."com.apple.quicktime.creationdate",
         .format.duration,
         .format.tags."com.apple.quicktime.location.ISO6709"
         ] | @csv'
done
