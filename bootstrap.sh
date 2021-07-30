#!/bin/bash -e

#
# Configuration
#
DMGDIR=dist/dmg

L4JVER=3.14
L4JDIR=dist/launch4j

JDKDIR=dist/jdks

#
# Donwload and unpack JDKs
#
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

# Windows x86_32
curl -O 'https://cdn.azul.com/zulu/bin/zulu16.32.15-ca-jdk16.0.2-win_i686.zip'
unzip zulu16.32.15-ca-jdk16.0.2-win_i686.zip
mv zulu16.32.15-ca-jdk16.0.2-win_i686 windows-x86_32

# Windows x86_64
curl -O 'https://cdn.azul.com/zulu/bin/zulu16.32.15-ca-jdk16.0.2-win_x64.zip'
unzip zulu16.32.15-ca-jdk16.0.2-win_x64.zip
mv zulu16.32.15-ca-jdk16.0.2-win_x64 windows-x86_64

# Windows aarch64
curl -O 'https://cdn.azul.com/zulu/bin/zulu16.30.17-ca-jdk16.0.1-win_aarch64.zip'
unzip zulu16.30.17-ca-jdk16.0.1-win_aarch64.zip
mv zulu16.30.17-ca-jdk16.0.1-win_aarch64 windows-aarch64

# Mac x86_64
curl -O 'https://cdn.azul.com/zulu/bin/zulu16.32.15-ca-jdk16.0.2-macosx_x64.tar.gz'
mkdir mac-x86_64
tar -C mac-x86_64 --strip-components=1 -xvf zulu16.32.15-ca-jdk16.0.2-macosx_x64.tar.gz

# Mac aarch64
curl -O 'https://cdn.azul.com/zulu/bin/zulu16.32.15-ca-jdk16.0.2-macosx_aarch64.tar.gz'
mkdir mac-aarch64
tar -C mac-aarch64 --strip-components=1 -xvf zulu16.32.15-ca-jdk16.0.2-macosx_aarch64.tar.gz

popd

#
# Download and unpack launch4j
#
mkdir -p "$L4JDIR"
pushd "$L4JDIR"

wget https://downloads.sourceforge.net/project/launch4j/launch4j-3/${L4JVER}/launch4j-${L4JVER}-linux-x64.tgz
tar -xvf launch4j-${L4JVER}-linux-x64.tgz

popd

#
# Compile dmg
#
mkdir -p "$DMGDIR"
pushd "$DMGDIR"

if [ ! -d libdmg-hfsplus ]; then
  git clone https://github.com/vassalengine/libdmg-hfsplus.git
fi

pushd libdmg-hfsplus
cmake . -B build
make -C build/dmg VERBOSE=1
popd

popd

#
# Set up Maven Wrapper
#
mvn -N io.takari:maven:0.7.7:wrapper
