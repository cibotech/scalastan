#!/bin/bash
set -e
export STAN_VERSION=2.19.1
export PATH="$PATH:$HOME/cmdstan-$STAN_VERSION/bin:$HOME/sbt/bin"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$(dirname $(dirname $(which clang)))/lib"
export CMDSTAN_HOME="$HOME/cmdstan-$STAN_VERSION"
$CMDSTAN_HOME/bin/stanc --version
