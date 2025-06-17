#!/bin/bash

# Set working directory
cd /opt

# Install icepack dependencies
apt-get update
apt-get install -y \
  pkg-config \
  libftdi-dev \
  libffi-dev

# Install yosys dependencies
apt-get install -y \
  tcl-dev \
  clang \
  gawk \
  libreadline-dev \
  mercurial

# Install yosys
git clone https://github.com/YosysHQ/yosys.git yosys
cd yosys
make -j$(nproc)
make install
cd ..

# Install icepack
git clone https://github.com/cliffordwolf/icestorm.git icestorm
cd icestorm
make -j$(nproc)
make install
cd ..

# Install arachne-pnr
git clone https://github.com/YosysHQ/arachne-pnr.git arachne-pnr
cd arachne-pnr
make -j$(nproc)
make install
cd ..

# Install next-pnr dependencies
apt-get install -y \
  cmake \
  python3-dev \
  qt5-default \
  libboost-dev \
  libboost-filesystem-dev \
  libboost-thread-dev \
  libboost-program-options-dev \
  libboost-python-dev \
  libboost-iostreams-dev \
  libeigen3-dev

# Install next-pnr
git clone https://github.com/YosysHQ/nextpnr nextpnr
cd nextpnr
cmake -DARCH=ice40
make -j$(nproc)
make install
cd ..
