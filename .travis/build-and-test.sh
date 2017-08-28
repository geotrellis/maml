#!/bin/bash


./sbt -J-Xmx2G "project mamlJVM" test || { exit 1; }
./sbt -J-Xmx2G "project mamlJS" test || { exit 1; }
./sbt -J-Xmx2G "project mamlRDD" test || { exit 1; }
