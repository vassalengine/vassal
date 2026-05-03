#!/bin/bash -e

# --- Default release numbers ----------------------------------------
#
# Edit these for reasonable defaults
#
L4J_VERSION=3.50
LIPO_VERSION=0.8.4
TEMURIN_RELEASE=26+35
LIBERICA_RELEASE=26.0.1+10

# --- Variables used below -------------------------------------------
#
# Default values set here
#
ARCHS=
NO_ACTION=0
CLEAN=0
WRAPPER=1
BUILD_DMG=1
VERBOSE=0
CURL_SILENT=-s
WGET_SILENT=-q
UNZIP_SILENT=-q
TAR_VERBOSE=
MAKE_VERBOSE=
CMAKE_QUIET="-DCMAKE_MESSAGE_LOG_LEVEL=WARNING"

# --- Target directories ---------------------------------------------
DMGDIR=dist/dmg
L4JDIR=dist/launch4j
JDKDIR=dist/jdks
LIPODIR=dist/lipo


# --- Message --------------------------------------------------------
#
# First argument is verbosity level
# Rest are message arguments to echo
#
msg() {
    lvl=$1 ; shift
    if [ $VERBOSE -lt $lvl ] ; then
        return
    fi

    echo "${@}"
}


# --- help -----------------------------------------------------------
usage() {
    cat <<-EOF
	Usage: ${0} [OPTIONS] [ARCHITECTURES]

	Options:
	  -h,--help                      Show this help and exit
	  -l,--l4j-version      VERSION  Set the Launch4J version to get
	  -i,--lipo-version     VERSION  Set the Lipo version to get
	  -j,--jre-release      RELEASE  Set the API version of the JREs to get
	  -t,--temurin-release  RELEASE  Temurin JRE release+build number
	  -b,--bellsoft-release RELEASE  Bellsoft JRE release+build number
	     --liberica-release RELEASE  Bellsoft JRE release+build number
	  -v,--verbose                   Increase verbosity
             --no-action                 Do nothing, show what to do
             --clean                     Removes everyting
             --no-wrapper                Do not update Maven wrapper
             --no-dmg                    Do not build libdmg-hfplus

        RELEASE is the full release number, including build number of one of
        the builds of OpenJDK.  It defaults to
	= Temurin: ${TEMURIN_RELEASE}
	= Liberica: ${LIBERICA_RELEASE}
        The option '--jre-release' sets both. 

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
        -j|--jre-release)      TEMURIN_RELEASE="$2"
                               LIBERICA_RELEASE="$2"; shift ;;
        -t|--temurin-release)  TEMURIN_RELEASE="$2" ; shift ;;
        -b|--bellsoft-release) LIBERICA_RELEASE="$2" ; shift ;;
        --liberica-release)    LIBERICA_RELEASE="$2" ; shift ;;
        --no-action)         NO_ACTION=1 ;;
        --clean)             CLEAN=1 ;;
        --no-wrapper)        WRAPPER=0 ;;
        --no-dmg)            BUILD_DMG=0 ;;
        -v|--verbose)        VERBOSE=$(($VERBOSE + 1)) ;;
        *)                   ARCHS="${ARCHS} $1" ;;
    esac
    shift
done

# --- Define prefix depending on action setting ----------------------
act=
if [ $NO_ACTION -gt 0 ] ; then 
    act=echo
fi

# --- Verbosity ------------------------------------------------------
if [ $VERBOSE -ge 3 ] ; then
    CURL_SILENT=
    UNZIP_SILENT=
    WGET_SILENT=
    TAR_VERBOSE=-v
    MAKE_VERBOSE=1
    CMAKE_QUIET=
fi

# --- Fix up a release number ----------------------------------------
fix_release() {
    release=$1 ; shift

    build=$(echo "$release" | sed -n 's/.*+\([0-9][0-9]*\)/\1/p')
    version=$(echo "$release" | sed 's/\([0-9][0-9.]*\).*/\1/')
    case x$version in
        x*.0.0) version=$(echo "$version" | sed 's/\..*//') ;; # 
        x*) ;;
    esac
    echo "${version}+${build}"
}
    
# --- See if we got a build number f.ex. from GitHub actions ---------
TEMURIN_RELEASE=$(fix_release "$TEMURIN_RELEASE")
LIBERICA_RELEASE=$(fix_release "$LIBERICA_RELEASE")

# --- Default architectures ------------------------------------------
if [ "x$ARCHS" = "x" ] ; then
    msg 1 "No architectures specified, getting them all"
    ARCHS="windows-x86_64 windows-x86_32 windows-aarch64 macos-x86_64 macos-aarch64"
fi

# ---- Unpack downloaded archive -------------------------------------
unpack_archive() {
    filename=$1 ; shift
    target=$1 ; shift

    if [ $CLEAN -gt 0 ] ; then
        rm -rf "${target}"
        return 0
    fi

    if [ -d "${target}" ] ; then
        return 0
    fi

    case x$filename in
        x*.zip)
            parent=$(dirname "$target")
            base=$(unzip -l "${filename}" | head -n4 | tail -n1 | sed -e 's/.* \([^ ][^ ]*\)/\1/' -e 's,/$,,')
            msg 1 "Unpacking ${filename} (base=${base})"
            $act mkdir -p "${parent}"
            $act rm -rf "${target}"
            $act unzip ${UNZIP_SILENT} "$filename"
            msg 2 "Moving ${base} to ${target}"
            $act mv "${base}" "${target}"
            ;;
        x*.tar.gz)
            $act mkdir -p ${target}
            msg 1 "Unpacking ${filename} to ${target}"
            $act tar ${TAR_VERBOSE} -C "${target}" --strip-components=1 -xf "${filename}"
            ;;
        x*)
            msg 0 "Unknown file type: $filenane" > dev/stderr
            ;;
    esac
    
    if [ ! -d "$target" ] && [ $NO_ACTION -lt 1 ]; then 
        echo "Failed to unpack ${filename} to ${target}" > /dev/stderr 
        exit 1
    fi
}
    

    
# ---- Get Temurin URL and download ----------------------------------
get_temurin_jdk() {
    release=$1 ; shift
    os=$1 ; shift
    arch=$1 ; shift
    target=$1 ; shift

    req="https://api.adoptium.net/v3/binary/version/jdk-${release}/${os}/${arch}/jmods/hotspot/normal/eclipse?project=jdk"
    msg 2 "Query for temurin download: ${req}"
    url=$(curl ${CURL_SILENT} -i "$req" | sed -n 's/^location: *//p' | tr -d '\r\n')
    msg 2 "Response: ${url}"
    filename=$(basename "$url")

    if [ "x$url" = "x" ] ; then
        echo "Failed get to Temurin download URL for ${release}/${os}/${arch}" > /dev/stderr
        exit 1
    fi

    if [ $CLEAN -gt 0 ] ; then
        $act rm -rf ${target}
        $act rm -rf ${filename}
        return 0
    fi
       
    if [ ! -f "$filename" ] ; then
        msg 1 "Downloading ${url}"
        $act curl ${CURL_SILENT} -O -L "${url}" -o "${filename}"
        if [ ! -f "${filename}" ] ; then
            echo "Failed to download ${url} to ${filename}" >/dev/stderr
            exit 1
        fi
        $act rm -rf ${target}
    fi

    unpack_archive "$filename" "$target"
}
            
# ---- Get Temurin URL and download ----------------------------------
get_liberica_jdk() {
    release=$1 ; shift
    os=$1 ; shift
    arch=$1 ; shift
    bits=$1 ; shift
    target=$1 ; shift

    req="https://api.bell-sw.com/v1/liberica/releases?version=${release}&bitness=${bits}&os=${os}&arch=${arch}&package-type=zip&bundle-type=jdk&output=text&fields=downloadUrl"
    msg 2 "Query for liberica download: ${req}"
    url=$(curl ${CURL_SILENT} "$req")
    msg 2 "Response: ${url}"
    case x$url in
        x*errorDescription*|x)
            echo "Failed to get Liberica download URL for ${release}/${os}/${arch}/${bits}" > /dev/stderr
            ;;
    esac
    filename=$(basename "$url")

    if [ $CLEAN -gt 0 ] ; then
        $act rm -rf ${target}
        $act rm -rf ${filename}
        return 0
    fi
       
    if [ ! -f "$filename" ] ; then
        msg 1 "Downloading ${url}"
        $act curl ${CURL_SILENT} -O -L "${url}" -o "${filename}"
        if [ ! -f "${filename}" ] ; then
            echo "Failed to download ${url} to ${filename}" >/dev/stderr
            exit 1
        fi
        $act rm -rf ${target}
    fi

    unpack_archive "$filename" "$target"
}

# --- Start downloading JREs -----------------------------------------
mkdir -p "$JDKDIR"
pushd "$JDKDIR"

for arch in $ARCHS ; do
    msg 3 "Getting architecture $arch"
    
    case $arch in
        windows-x86_32|win32)
            get_liberica_jdk 			\
                "21.0.10+10" 			\
                windows 			\
                x86 				\
                32 				\
                "windows-x86_32"
            ;;
        windows-x86_64|win64)
            get_temurin_jdk 			\
                "${TEMURIN_RELEASE}"	 	\
                windows 			\
                x64				\
                "windows-x86_64/jmods"
            ;;
        windows-aarch64|winarm)
            get_liberica_jdk 			\
                "${LIBERICA_RELEASE}" 			\
                windows 			\
                arm				\
                64 				\
                "windows-aarch64"
            ;;
        macos-x86_64|macintel)
            get_temurin_jdk 			\
                "${TEMURIN_RELEASE}" 		\
                mac 				\
                x64 				\
                "macos-x86_64/Contents/Home/jmods"
            ;;
        macos-aarch64|mac)
            get_temurin_jdk 			\
                "${TEMURIN_RELEASE}" 		\
                mac 				\
                aarch64 			\
                "macos-aarch64/Contents/Home/jmods"
            ;;
        *)
            echo "Unknown architecture: ${arch} - ignored" > /dev/stderr
            ;;
    esac
done 

popd

# --- Download lipo --------------------------------------------------
if [ "x$LIPO_VERSION" != "x" ] ; then 
    mkdir -p "$LIPODIR"
    pushd "$LIPODIR"
    
    if [ $CLEAN -gt 0 ] ; then
        $act rm -f lipo_linux_amd64
    else
        msg 1 "Downloading https://github.com/konoui/lipo/releases/download/v${LIPO_VERSION}/lipo_linux_amd64"
        $act wget ${WGET_SILENT} https://github.com/konoui/lipo/releases/download/v${LIPO_VERSION}/lipo_linux_amd64
        $act chmod a+x lipo_linux_amd64
    fi
    
    popd
fi

# --- Download and unpack launch4j -----------------------------------
if [ x$L4J_VERSION != x ] ; then 
    mkdir -p "$L4JDIR"
    pushd "$L4JDIR"

    if [ $CLEAN -gt 0 ] ; then
        $act rm -f launch4j-${L4J_VERSION}-linux-x64.tgz
        $act rm -rf launch4j
    else
        msg 1 "Downloading https://downloads.sourceforge.net/project/launch4j/launch4j-3/${L4J_VERSION}/launch4j-${L4J_VERSION}-linux-x64.tgz"
        $act wget ${WGET_SILENT} https://downloads.sourceforge.net/project/launch4j/launch4j-3/${L4J_VERSION}/launch4j-${L4J_VERSION}-linux-x64.tgz
        msg 2 "Extracting launch4j-${L4J_VERSION}-linux-x64.tgz"
        $act tar ${TAR_VERBOSE} -xf launch4j-${L4J_VERSION}-linux-x64.tgz
        # check if script is still broken in next release after 3.50
        # see https://sourceforge.net/p/launch4j/bugs/227/
        msg 3 "Fixing line-endings in launch4j"
        $act dos2unix launch4j/launch4j || true
    fi

    popd
fi

# --- Compile dmg ----------------------------------------------------
if [ $BUILD_DMG -gt 0 ] ; then 
    mkdir -p "$DMGDIR"
    pushd "$DMGDIR"

    if [ $CLEAN -gt 0 ] ; then
        $act rm -rf libdmg-hfsplus
    else
        if [ ! -d libdmg-hfsplus ]; then
            msg 1 "Cloning https://github.com/vassalengine/libdmg-hfsplus.git"
            $act git clone https://github.com/vassalengine/libdmg-hfsplus.git
        fi

        msg 1 "Building libdmg-hfsplus"
        $act pushd libdmg-hfsplus
        $act cmake . -B build ${CMAKE_QUIET}
        $act make -C build/dmg VERBOSE=${MAKE_VERBOSE}
        $act popd
    fi

    popd
fi

# --- Set up Maven Wrapper -------------------------------------------
if [ $WRAPPER -gt 0 ] ; then
    msg 1 "Updating Maven wrapper"
    $act mvn wrapper:wrapper
fi

#
# EOF
#

