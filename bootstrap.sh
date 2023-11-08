#!/bin/bash -e

#
# Configuration
#
DMGDIR=dist/dmg

L4JVER=3.50
L4JDIR=dist/launch4j

JDKDIR=dist/jdks

LIPOVER=0.8.4
LIPODIR=dist/lipo

#
# Donwload and unpack JDKs
#
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

ZULU_URL='https://cdn.azul.com/zulu/bin'

TEMURIN_URL='https://github.com/adoptium/temurin21-binaries/releases/download'
TEMURIN_VERSION=jdk-21+35
TEMURIN_FILENAME_VERSION=21_35

BELLSOFT_URL='https://download.bell-sw.com/java/21%2B37'
BELLSOFT_VERSION=21+37
BELLSOFT_DIR=jdk-21

# Windows x86_32
filename="bellsoft-jdk$BELLSOFT_VERSION-windows-i586.zip"
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv $BELLSOFT_DIR windows-x86_32

# Windows x86_64
filename="OpenJDK21U-jdk_x64_windows_hotspot_$TEMURIN_FILENAME_VERSION.zip"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
unzip $filename
mv $TEMURIN_VERSION windows-x86_64

# Windows aarch64
filename="bellsoft-jdk$BELLSOFT_VERSION-windows-aarch64.zip"
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv $BELLSOFT_DIR windows-aarch64

# MacOS x86_64
filename="OpenJDK21U-jdk_x64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir macos-x86_64
tar -C macos-x86_64 --strip-components=1 -xvf $filename

# MacOS aarch64
filename="OpenJDK21U-jdk_aarch64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir macos-aarch64
tar -C macos-aarch64 --strip-components=1 -xvf $filename

popd

#
# Download lipo
#
mkdir -p "$LIPODIR"
pushd "$LIPODIR"

wget https://github.com/konoui/lipo/releases/download/v${LIPOVER}/lipo_linux_amd64
chmod a+x lipo_linux_amd64

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
