#!/usr/bin/env bash
set -euo pipefail

# Install packages and tools for building T800

echo "=== Installing T800 build environment ==="
apt-get update
apt-get install -y software-properties-common curl gnupg2

add-apt-repository -y ppa:openjdk-r/ppa
apt-get update

curl -fsSL https://keyserver.ubuntu.com/pks/lookup?op=get\&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
  | gpg --dearmor -o /usr/share/keyrings/sbt.gpg
echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
  > /etc/apt/sources.list.d/sbt.list

apt-get update
apt-get install -y \
  openjdk-21-jdk sbt gtkwave git make autoconf g++ flex bison \
  help2man device-tree-compiler libboost-all-dev wget

# Build and install Verilator from source

tmp=/tmp/verilator
git clone --depth 1 https://github.com/verilator/verilator.git "$tmp"
cd "$tmp" && autoconf && ./configure && make -j$(nproc) && make install
rm -rf "$tmp"

# Display tool versions

java  -version | head -n1
sbt   --script-version
verilator --version
