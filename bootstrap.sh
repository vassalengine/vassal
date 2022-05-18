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

ZULU_URL='https://cdn.azul.com/zulu/bin'
ZULU_BASENAME=zulu18.30.11-ca-jdk18.0.1

# Windows x86_32
filename=$ZULU_BASENAME-win_i686.zip
curl -O "$ZULU_URL/$filename"
unzip $filename
mv $(basename $filename .zip) windows-x86_32

# Windows x86_64
filename=$ZULU_BASENAME-win_x64.zip
curl -O "$ZULU_URL/$filename"
unzip $filename
mv $(basename $filename .zip) windows-x86_64

# Windows aarch64
filename=$ZULU_BASENAME-win_aarch64.zip
curl -O "$ZULU_URL/$filename"
unzip $filename
mv $(basename $filename .zip) windows-aarch64

# MacOS x86_64
filename=$ZULU_BASENAME-macosx_x64.tar.gz
curl -O "$ZULU_URL/$filename"
mkdir macos-x86_64
tar -C macos-x86_64 --strip-components=1 -xvf $filename

# MacOS aarch64
filename=$ZULU_BASENAME-macosx_aarch64.tar.gz
curl -O "$ZULU_URL/$filename"
mkdir macos-aarch64
tar -C macos-aarch64 --strip-components=1 -xvf $filename

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
