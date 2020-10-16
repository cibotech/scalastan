#!/usr/bin/env bash
set -e
cd $HOME
echo "Home directory => $HOME"
# Install CmdStan
if [ ! -x cmdstan-$STAN_VERSION/bin/stanc ] ; then
  echo "Downloading STAN from https://github.com/stan-dev/cmdstan/releases/download/v$STAN_VERSION/cmdstan-$STAN_VERSION.tar.gz"
  wget https://github.com/stan-dev/cmdstan/releases/download/v$STAN_VERSION/cmdstan-$STAN_VERSION.tar.gz
  tar -xzf cmdstan-$STAN_VERSION.tar.gz
  (cd cmdstan-$STAN_VERSION && make build && echo "STAN installed in $(cd)")
fi
