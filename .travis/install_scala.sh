#!/usr/bin/env bash

set -ve

SCALA_FILE="scala-$SCALA_VERSION.deb"
SBT_FILE="sbt-$SBT_VERSION.deb"

wget https://www.scala-lang.org/files/archive/$SCALA_FILE && sudo dpkg -i $SCALA_FILE
wget https://bintray.com/artifact/download/sbt/debian/$SBT_FILE && sudo dpkg -i $SBT_FILE
