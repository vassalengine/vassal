#!/bin/bash -e

#
# Configuration
#
DMGDIR=dist/dmg

L4JVER=3.50
L4JDIR=dist/launch4j

JDKDIR=dist/jdks

#
# Donwload and unpack JDKs
#
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

ZULU_URL='https://cdn.azul.com/zulu/bin'

TEMURIN_URL='https://github.com/adoptium/temurin20-binaries/releases/download'
TEMURIN_VERSION=jdk-20+36

BELLSOFT_URL='https://download.bell-sw.com/java/20%2B37'

# Windows x86_32
filename=bellsoft-jdk20+37-windows-i586.zip
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv jdk-20 windows-x86_32

# Windows x86_64
filename=OpenJDK20U-jdk_x64_windows_hotspot_20_36.zip
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
unzip $filename
mv $TEMURIN_VERSION windows-x86_64

# Windows aarch64
filename=bellsoft-jdk20+37-windows-aarch64.zip
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv jdk-20 windows-aarch64

# MacOS x86_64
filename=OpenJDK20U-jdk_x64_mac_hotspot_20_36.tar.gz
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir macos-x86_64
tar -C macos-x86_64 --strip-components=1 -xvf $filename

# MacOS aarch64
filename=OpenJDK20U-jdk_aarch64_mac_hotspot_20_36.tar.gz
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
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
# check if script is still broken in next release after 3.50
# see https://sourceforge.net/p/launch4j/bugs/227/
dos2unix launch4j/launch4j
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
mvn wrapper:wrapper
