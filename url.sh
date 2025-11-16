#! /bin/bash
set -e
V=${1:-1.4.0}
url="https://github.com/lindig/fit/archive/$V.zip"
test -f $V.zip || wget "$url"
cat <<EOF
url {
  src: "$url"
  checksum: [
    "sha256=$(sha256 -q $V.zip)"
    "md5=$(md5 -q $V.zip)"
  ]
}
EOF
