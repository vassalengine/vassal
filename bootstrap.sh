#!/bin/bash -e

#
# Configuration
#
DMGDIR=dist/dmg
L4JDIR=dist/launch4j

JDKVER="jdk-16+36"
JDKDIR=dist/jdks

#
# Donwload and unpack JDKs
#
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

for i in mac,x64 windows,x64 windows,x32 ; do
  IFS=',' read os arch <<<"$i"

  real_url=$(curl -s -w '%{redirect_url}' -X GET "https://api.adoptopenjdk.net/v3/binary/version/${JDKVER}/${os}/${arch}/jdk/hotspot/normal/adoptopenjdk?project=jdk")

  real_filename=${real_url##*/}
  if [ ! -f "$real_filename" ]; then
    echo "Downloading $os $arch JDK..."
    curl -X GET "$real_url" -L -o "$real_filename"
  fi

  if [ ! -d "${os}_${arch}" ]; then
    echo "Unpacking $real_filename..."
    extract_dir="${os}_${arch}"
    mkdir "$extract_dir"  
    if [ "$os" = "windows" ]; then 
      unzip -d "$extract_dir" "$real_filename"
      # top-level directory in windows archives is the full version number;
      # flatten that to get a predictable path
      f=("$extract_dir"/*)
      mv "$extract_dir"/*/* "$extract_dir"
      rmdir "${f[@]}"
    else
      tar -C "$extract_dir" --strip-components=1 -xvf "$real_filename"
    fi
  fi
done

popd

#
# Download and unpack launch4j
#
mkdir -p "$L4JDIR"
pushd "$L4JDIR"

wget https://downloads.sourceforge.net/project/launch4j/launch4j-3/3.13/launch4j-3.13-linux-x64.tgz
tar -xvf launch4j-3.13-linux-x64.tgz

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
