#!/bin/bash
#

set -e

ffprobe -v quiet -print_format json -show_format "$1" |\
  jq -r '.format.tags."com.apple.quicktime.creationdate"'
