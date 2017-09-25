#!/usr/bin/env bash

set -e
set -x

./sbt "++$TRAVIS_SCALA_VERSION" "project mamlJVM" publish
./sbt "++$TRAVIS_SCALA_VERSION" "project mamlJS" publish
./sbt "++$TRAVIS_SCALA_VERSION" "project mamlSpark" publish

