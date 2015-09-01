#!/usr/bin/env bash

set -o errexit

if [ ! "$1" ] ; then
  echo Please, specify the remote port as the first argument.
  exit 1
fi

if [ ! "$2" ] ; then
  echo Please, specify the local port as the first argument.
  exit 1
fi

ssh -R "0.0.0.0:$1:localhost:$2" -NT cufp@jkarni.com
