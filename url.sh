#! /bin/bash
set -e
V=${1:-1.4.1}
url="https://github.com/lindig/fit/archive/$V.zip"
test -f $V.zip || wget "$url"
cat <<EOF
url {
  src: "$url"
  checksum: [
    "sha256=$(sha256 -q $V.zip || sha256sum $V.zip | awk '{print $1}')"
    "md5=$(md5 -q $V.zip || md5sum $V.zip | awk '{print $1}')"
  ]
}
EOF
