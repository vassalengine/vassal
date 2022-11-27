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

TEMURIN_URL='https://github.com/adoptium/temurin19-binaries/releases/download'
TEMURIN_VERSION=jdk-19.0.1+10

BELLSOFT_URL='https://download.bell-sw.com/java/19.0.1%2B11'


# Windows x86_32
filename=OpenJDK19U-jdk_x86-32_windows_hotspot_19.0.1_10.zip
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
unzip $filename
mv $TEMURIN_VERSION windows-x86_32

# Windows x86_64
filename=OpenJDK19U-jdk_x64_windows_hotspot_19.0.1_10.zip
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
unzip $filename
mv $TEMURIN_VERSION windows-x86_64

# Windows aarch64
filename=bellsoft-jdk19.0.1+11-windows-aarch64.zip
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv jdk-19.0.1 windows-aarch64

# MacOS x86_64
filename=OpenJDK19U-jdk_x64_mac_hotspot_19.0.1_10.tar.gz
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir macos-x86_64
tar -C macos-x86_64 --strip-components=1 -xvf $filename

# MacOS aarch64
filename=OpenJDK19U-jdk_aarch64_mac_hotspot_19.0.1_10.tar.gz
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
