#!/bin/bash

set -e  # Exit on error

# Set working directory
cd /opt || { echo "Failed to cd to /opt"; exit 1; }

# Update package lists
sudo apt-get update

# Install icepack dependencies
sudo apt-get install -y \
  pkg-config \
  libftdi-dev \
  libffi-dev

# Install yosys dependencies
sudo apt-get install -y \
  tcl-dev \
  clang \
  gawk \
  libreadline-dev \
  mercurial

# Install yosys
git clone https://github.com/YosysHQ/yosys.git yosys || { echo "Yosys clone failed"; exit 1; }
cd yosys || { echo "Failed to cd to yosys"; exit 1; }
git submodule update --init --recursive
make -j$(nproc)
sudo make install
cd ..

# Install icepack
git clone https://github.com/cliffordwolf/icestorm.git icestorm || { echo "Icestorm clone failed"; exit 1; }
cd icestorm || { echo "Failed to cd to icestorm"; exit 1; }
make -j$(nproc)
sudo make install
cd ..

# Install arachne-pnr
git clone https://github.com/YosysHQ/arachne-pnr.git arachne-pnr || { echo "Arachne-pnr clone failed"; exit 1; }
cd arachne-pnr || { echo "Failed to cd to arachne-pnr"; exit 1; }
make -j$(nproc)
sudo make install
cd ..

# Install next-pnr dependencies
sudo apt-get install -y \
  cmake \
  python3-dev \
  qtbase5-dev \
  libboost-dev \
  libboost-filesystem-dev \
  libboost-thread-dev \
  libboost-program-options-dev \
  libboost-python-dev \
  libboost-iostreams-dev \
  libeigen3-dev

# Install next-pnr
git clone https://github.com/YosysHQ/nextpnr nextpnr || { echo "Nextpnr clone failed"; exit 1; }
cd nextpnr || { echo "Failed to cd to nextpnr"; exit 1; }
git submodule update --init --recursive
cmake -B build -DARCH=ice40
cmake --build build -j$(nproc)
sudo make install

cd $HOME

yosys --version
icepack --version
arachne-pnr --version
nextpnr-ice40 --version
