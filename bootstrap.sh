#!/bin/bash -e

L4J_VERSION=3.50
LIPO_VERSION=0.8.4
JRE_API=26
TEMURIN_BUILD=35
BELLSOFT_BUILD=37
ARCHS=
NO_ACTION=0
CLEAN=0

# --- help -----------------------------------------------------------
usage() {
    cat <<-EOF
	Usage: ${0} [OPTIONS] [ARCHITECTURES]

	Options:
	  -h,--help                    Show this help and exit
	  -l,--l4j-version    VERSION  Set the Launch4J version to get
	  -i,--lipo-version   VERSION  Set the Lipo version to get
	  -j,--jre-version    VERSION  Set the API version of the JREs to get
	  -t,--temurin-build  NUMBER   Temurin JRE build number
	  -b,--bellsoft-build NUMBER   Bellsoft JRE build number
          
	ARCHITECTURES is one or more of
	  windows-x86_64      Windows on Intel 64 bit
	  windows-x86_32      Windows on Intel 32 bit(*)
	  windows-aarch64     Windows on ARM 64 bit
	  macos-x86_64        MacOS on intel 64 bit
	  macos-aarch64       MacOS on ARM 64 bit

	If no ARCHITCTURES are specified, then it will set to all
	of the above. 

	(*) The JRE API and Bellsoft build of Windows on Intel 32 bit
	are fixed at 21.0.10 and 10, respectively.
	EOF
}

# --- Parse command line ---------------------------------------------    
while test $# -gt 0 ; do
    case $1 in
        -h|--help) usage ; exit ;;
        -l|--l4j-version)    L4J_VERSION=$2 ; shift ;;
        -i|--lipo-version)   LIPO_VERSION=$2 ; shift ;;
        -j|--jre-version)    JRE_API=$2 ; shift ;;
        -t|--temurin-build)  TEMURIN_BUILD=$2 ; shift ;;
        -b|--bellsoft-build) BELLSOFT_BUILD=$2 ; shift ;;
        --no-action)         NO_ACTION=1 ;;
        --clean)             CLEAN=1 ;;
        *)                   ARCHS="${ARCHS} $1" ;;
    esac
    shift
done

# --- Define prefix depending on action setting ----------------------
act=
if [ $NO_ACTION -gt 0 ] ; then
    act=echo
fi

# --- See if we got a build number f.ex. from GitHub actions ---------
BUILD=$(echo "$JRE_API" | sed -n 's/.*+\([0-9][0-9]*\)/\1/p')
if [ "x$BUILD" != "x" ] ; then
    TEMURIN_BUILD=$BUILD
    BELLSOFT_BUILD=$BUILD
fi

# --- Sanitise the JRE version we get in GitHub actions --------------
FULL_JRE=$(echo "$JRE_API" | sed 's/\([0-9][0-9.]*\).*/\1/')
JRE_API=$(echo "$JRE_API" | sed 's/\([0-9][0-9]*\)\..*/\1/')

# --- Default architectures ------------------------------------------
if [ "x$ARCHS" = "x" ] ; then
    echo "No architectures specified, getting them all"
    ARCHS="windows-x86_64 windows-x86_32 windows-aarch64 macos-x86_64 macos-aarch64"
fi

# --- Target directories ---------------------------------------------
#
DMGDIR=dist/dmg
L4JDIR=dist/launch4j
JDKDIR=dist/jdks
LIPODIR=dist/lipo

# --- JRE (JDK) settings ---------------------------------------------
TEMURIN_VERSION=jdk-${JRE_API}+${TEMURIN_BUILD}
TEMURIN_FILENAME_VERSION=${JRE_API}_${TEMURIN_BUILD}
TEMURIN_URL="https://github.com/adoptium/temurin${JRE_API}-binaries/releases/download/${TEMURIN_VERSION}"
TEMURIN_FILENAME_BASE="OpenJDK${JRE_API}U-jmods"

BELLSOFT_VERSION=${FULL_JRE}+${BELLSOFT_BUILD}
BELLSOFT_URL="https://download.bell-sw.com/java/${BELLSOFT_VERSION}"
BELLSOFT_DIR=jdk-${FULL_JRE}

BELLSOFT_WIN32_API=21.0.10
BELLSOFT_WIN32_BUILD=10
BELLSOFT_WIN32_VERSION=${BELLSOFT_WIN32_API}+${BELLSOFT_WIN32_BUILD}
BELLSOFT_WIN32_URL="https://download.bell-sw.com/java/${BELLSOFT_WIN32_VERSION}"
BELLSOFT_WIN32_DIR=jdk-${BELLSOFT_WIN32_API}

# ---- Download a JDK ------------------------------------------------
get_jdk() {
    filename="$1" ; shift
    url="$1" ; shift
    base="$1" ; shift 
    target="$1" ; shift
    
    if [ $CLEAN -gt 0 ] ; then
        $act rm -rf ${target}
        $act rm -rf ${filename}
        return 0
    fi
    
    if [ ! -f "$filename" ] ; then
        echo "Downloading ${url}/${filename}"
        $act curl -O -L "${url}/${filename}"
    fi

    if [ ! -d "$target" ] ; then 
        case "x$filename" in 
            x*.zip)
                parent=$(dirname "$target")
                $act mkdir -p $parent
                $act unzip "$filename"
                $act mv "$base" "$target"
                ;;
            x*.tar.gz)
                $act mkdir -p "${target}"
                $act tar -C "${target}" --strip-components=1 -xf "${filename}"
                ;;
            esac
    fi

    if [ ! -d "$target" ] && [ $NO_ACTION -lt 1 ]; then 
        echo "Failed to download ${url}/${filename} to ${target}" > /dev/stderr 
        exit 1
    fi
}

# --- Start downloading JREs -----------------------------------------
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

for arch in $ARCHS ; do
    # echo "Getting architecture $arch"
    
    case $arch in
        windows-x86_32|win32)
            get_jdk \
                "bellsoft-jdk${BELLSOFT_WIN32_VERSION}-windows-i586.zip" \
                "${BELLSOFT_WIN32_URL}" \
                "${BELLSOFT_WIN32_DIR}" \
                "windows-x86_32"
            ;;
        windows-x86_64|win64)
            get_jdk \
                "${TEMURIN_FILENAME_BASE}_x64_windows_hotspot_${TEMURIN_FILENAME_VERSION}.zip" \
                "${TEMURIN_URL}" \
                "${TEMURIN_VERSION}-jmods" \
                "windows-x86_64/jmods"
            ;;
        windows-aarch64|winarm)
            get_jdk \
                "bellsoft-jdk${BELLSOFT_VERSION}-windows-aarch64.zip" \
                "${BELLSOFT_URL}" \
                "${BELLSOFT_DIR}" \
                "windows-aarch64"
            ;;
        macos-x86_64|macintel)
            get_jdk \
                "${TEMURIN_FILENAME_BASE}_x64_mac_hotspot_${TEMURIN_FILENAME_VERSION}.tar.gz" \
                "${TEMURIN_URL}" \
                "" \
                "macos-x86_64/Contents/Home/jmods"
            ;;
        macos-aarch64|mac)
            get_jdk \
                "${TEMURIN_FILENAME_BASE}_aarch64_mac_hotspot_${TEMURIN_FILENAME_VERSION}.tar.gz" \
                "${TEMURIN_URL}" \
                "" \
                "macos-aarch64/Contents/Home/jmods"
            ;;
        *)
            echo "Unknown architecture: ${arch} - ignored" > /dev/stderr
            ;;
    esac
done 

popd

# --- Download lipo --------------------------------------------------
mkdir -p "$LIPODIR"
pushd "$LIPODIR"

if [ $CLEAN -gt 0 ] ; then
    rm -f lipo_linux_amd64
else
    $act wget https://github.com/konoui/lipo/releases/download/v${LIPO_VERSION}/lipo_linux_amd64
    $act chmod a+x lipo_linux_amd64
fi

popd

# --- Download and unpack launch4j -----------------------------------
mkdir -p "$L4JDIR"
pushd "$L4JDIR"

if [ $CLEAN -gt 0 ] ; then
    $act rm -f launch4j-${L4J_VERSION}-linux-x64.tgz
    $act rm -rf launch4j
else
    $act wget https://downloads.sourceforge.net/project/launch4j/launch4j-3/${L4J_VERSION}/launch4j-${L4J_VERSION}-linux-x64.tgz
    $act tar -xvf launch4j-${L4J_VERSION}-linux-x64.tgz
    # check if script is still broken in next release after 3.50
# see https://sourceforge.net/p/launch4j/bugs/227/
    $act dos2unix launch4j/launch4j || true
fi

popd

# --- Compile dmg ----------------------------------------------------
mkdir -p "$DMGDIR"
pushd "$DMGDIR"

if [ $CLEAN -gt 0 ] ; then
    $act rm -rf libdmg-hfsplus
else
    if [ ! -d libdmg-hfsplus ]; then
        $act git clone https://github.com/vassalengine/libdmg-hfsplus.git
    fi

    $act pushd libdmg-hfsplus
    $act cmake . -B build
    $act make -C build/dmg VERBOSE=1
    $act popd
fi

popd

# --- Set up Maven Wrapper -------------------------------------------
$act mvn wrapper:wrapper

#
# EOF
#

