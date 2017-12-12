#!/usr/bin/env bash

cd $HOME

# Install CmdStan
if [ ! -x cmdstan-$STAN_VERSION/bin/stanc ] ; then
  wget https://github.com/stan-dev/cmdstan/releases/download/v$STAN_VERSION/cmdstan-$STAN_VERSION.tar.gz
  tar -xzf cmdstan-$STAN_VERSION.tar.gz
  (cd cmdstan-$STAN_VERSION && make build)
fi
