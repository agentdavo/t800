#!/usr/bin/env bash

set -e

if [ ! -f "ext/SpinalHDL/build.sbt" ]; then
    echo "Error: submodule 'ext/SpinalHDL' not initialized."
    echo "Run 'git submodule update --init --recursive' to fetch it." >&2
    exit 1
fi

echo "SpinalHDL submodule present."
