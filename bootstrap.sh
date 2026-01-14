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

TEMURIN_URL='https://github.com/adoptium/temurin25-binaries/releases/download'
TEMURIN_VERSION=jdk-25+36
TEMURIN_FILENAME_VERSION=25_36

BELLSOFT_URL='https://download.bell-sw.com/java/25%2B37'
BELLSOFT_VERSION=25+37
BELLSOFT_DIR=jdk-25

BELLSOFT_WIN32_URL='https://download.bell-sw.com/java/21.0.8%2B12'
BELLSOFT_WIN32_VERSION=21.0.8+12
BELLSOFT_WIN32_DIR=jdk-21.0.8

# TODO check for downloads

# Windows x86_32
filename="bellsoft-jdk$BELLSOFT_WIN32_VERSION-windows-i586.zip"
if [ ! -f "$filename" ] ; then
  curl -O "$BELLSOFT_WIN32_URL/$filename"
fi
if [ ! -d windows-x86_32 ] ; then
  unzip $filename
  mv $BELLSOFT_WIN32_DIR windows-x86_32
fi

# Windows x86_64
filename="OpenJDK25U-jmods_x64_windows_hotspot_$TEMURIN_FILENAME_VERSION.zip"
if [ ! -f "$filename" ] ; then
  curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
fi
if [ ! -d windows-x86_64 ] ; then
  unzip $filename
  mkdir -p windows-x86_64
  mv $TEMURIN_VERSION-jmods windows-x86_64/jmods
fi

# Windows aarch64
filename="bellsoft-jdk$BELLSOFT_VERSION-windows-aarch64.zip"
if [ ! -f "$filename" ] ; then
  curl -O "$BELLSOFT_URL/$filename"
fi
if [ ! -d windows-aarch64 ] ; then
  unzip $filename
  mv $BELLSOFT_DIR windows-aarch64
fi

# MacOS x86_64
filename="OpenJDK25U-jmods_x64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
if [ ! -f "$filename" ] ; then
  curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
fi
if [ ! -d macos-x86_64 ] ; then
  mkdir -p macos-x86_64/Contents/Home/jmods
  tar -C macos-x86_64/Contents/Home/jmods --strip-components=1 -xvf $filename
fi

# MacOS aarch64
filename="OpenJDK25U-jmods_aarch64_mac_hotspot_$TEMURIN_FILENAME_VERSION.tar.gz"
if [ ! -f "$filename" ] ; then
  curl -L -O "$TEMURIN_URL/$TEMURIN_VERSION/$filename"
fi
if [ ! -d macos-aarch64 ] ; then
  mkdir -p macos-aarch64/Contents/Home/jmods
  tar -C macos-aarch64/Contents/Home/jmods --strip-components=1 -xvf $filename
fi

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
