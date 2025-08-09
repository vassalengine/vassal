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

TEMURIN_URL='https://github.com/adoptium/temurin24-binaries/releases/download'
TEMURIN_VERSION=jdk-24.0.2+12
TEMURIN_FILENAME_VERSION=24.0.2_12

BELLSOFT_URL='https://download.bell-sw.com/java/24.0.2%2B12'
BELLSOFT_VERSION=24.0.2+12
BELLSOFT_DIR=jdk-24.0.2

BELLSOFT_WIN32_URL='https://download.bell-sw.com/java/21.0.8%2B12'
BELLSOFT_WIN32_VERSION=21.0.8+12
BELLSOFT_WIN32_DIR=jdk-21.0.8

# Windows x86_32
filename="bellsoft-jdk$BELLSOFT_WIN32_VERSION-windows-i586.zip"
curl -O "$BELLSOFT_WIN32_URL/$filename"
unzip $filename
mv $BELLSOFT_WIN32_DIR windows-x86_32

# Windows x86_64
filename="OpenJDK24U-jmods_x64_windows_hotspot_$TEMURIN_FILENAME_VERSION.zip"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
unzip $filename
mkdir windows-x86_64
mv $TEMURIN_VERSION-jmods windows-x86_64/jmods

# Windows aarch64
filename="bellsoft-jdk$BELLSOFT_VERSION-windows-aarch64.zip"
curl -O "$BELLSOFT_URL/$filename"
unzip $filename
mv $BELLSOFT_DIR windows-aarch64

# MacOS x86_64
filename="OpenJDK24U-jmods_x64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir -p macos-x86_64/Contents/Home/jmods
tar -C macos-x86_64/Contents/Home/jmods --strip-components=1 -xvf $filename

# MacOS aarch64
filename="OpenJDK24U-jmods_aarch64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
mkdir -p macos-aarch64/Contents/Home/jmods
tar -C macos-aarch64/Contents/Home/jmods --strip-components=1 -xvf $filename

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
