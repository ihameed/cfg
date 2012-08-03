#!/bin/sh
cd $1
export EXTRA_CONFIGURE_OPTS=--enable-library-profiling
sh bootstrap.sh
