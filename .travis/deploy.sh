#!/usr/bin/env bash

set -e
set -x

mkdir -p "${HOME}/.bintray"

cat <<EOF > "${HOME}/.bintray/.credentials"
realm = Bintray API Realm
host = api.bintray.com
user = $BINTRAY_USER
password = $BINTRAY_PASS
EOF

./sbt "++$TRAVIS_SCALA_VERSION" -Dbintray.user=$BINTRAY_USER -Dbintray.pass=$BINTRAY_PASS "project mamlJVM" publish
./sbt "++$TRAVIS_SCALA_VERSION" -Dbintray.user=$BINTRAY_USER -Dbintray.pass=$BINTRAY_PASS "project mamlJS" publish
./sbt "++$TRAVIS_SCALA_VERSION" -Dbintray.user=$BINTRAY_USER -Dbintray.pass=$BINTRAY_PASS "project mamlSpark" publish
