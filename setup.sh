#!/usr/bin/env bash

set -o errexit

if ! which stack ; then
  echo installing stack into $HOME/.local/bin
  mkdir -p "$HOME/.local/bin"
  curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-linux.gz | gunzip > ~/.local/bin/stack
  chmod a+x ~/.local/bin/stack
  echo Please, put $HOME/.local/bin in your \$PATH.
  stack --version
else
  echo found stack
  stack --version
fi

stack setup
stack build --pedantic
