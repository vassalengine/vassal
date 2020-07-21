#!/bin/bash -e

#
# Configuration
#
DMGDIR=dmg

JDKVER=14
JDKDIR=jdks

#
# Donwload and unpack JDKs
#
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

for i in mac,x64 windows,x64 windows,x32 ; do
  IFS=',' read os arch <<<"$i"

  real_url=$(curl -s -w '%{redirect_url}' -X GET "https://api.adoptopenjdk.net/v3/binary/latest/${JDKVER}/ga/${os}/${arch}/jdk/hotspot/normal/adoptopenjdk?project=jdk")

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
# Compile dmg
#
mkdir -p "$DMGDIR"
pushd "$DMGDIR"

if [ ! -d libdmg-hfsplus ]; then
  git clone https://github.com/uckelman/libdmg-hfsplus.git
fi

pushd libdmg-hfsplus
cmake . -B build
make -C build/dmg VERBOSE=1
popd

popd
