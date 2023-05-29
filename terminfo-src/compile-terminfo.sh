#!/bin/sh

ROOT="$(cd "$(dirname "$0")"/..; pwd -P)"

cd "$ROOT"

exec tic -o "$ROOT/terminfo" "$ROOT/terminfo-src/xterm-256color"
