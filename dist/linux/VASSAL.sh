#!/bin/sh

#
# Execute this file to launch VASSAL on Linux
#

JAVA=$(which java)

# Check that java exists
if [ ! -x "$JAVA" ]; then
  echo "Error: $JAVA cannot be run. Please ensure that Java is installed." 2>&1
  exit 1
fi

# Check the Java vesion; we require Java 11+.
VERSION=$("$JAVA" -version 2>&1 | grep version | sed -e 's/.* version "//; s/".*//')

# Java 8 and earlier formats version x.y as 1.x.y
# Java 9 and later formats version x.y as x.y
MAJOR=$(echo "$VERSION" | sed -e 's/\..*//')
if [ "$MAJOR" = '1' ]; then
  MAJOR=$(echo "$VERSION" | sed -e 's/[0-9]\+\.//; s/[^0-9].*//')
fi

# Check that the major version we found is an integer
MAJOR=$(echo "$MAJOR" | sed -e 's/[^0-9]//g')
if [ -z "$MAJOR" ]; then
  echo "Error: VASSAL requires Java 11 or later. The Java you are using ($JAVA) has an unrecognizable version '$VERSION'." 2>&1
  exit 1
fi

# Check that the major version we found is >= 11
if [ "$MAJOR" -lt 11 ]; then
  echo "Error: VASSAL requires Java 11 or later. The Java you are using ($JAVA) is Java $MAJOR." 2>&1
  exit 1
fi

# Find absolute path where VASSAL is installed
INSTALL_DIR=$(cd "$(dirname "$0")"; pwd)

# Launch VASSSAL
"$JAVA" -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar --add-exports java.desktop/sun.java2d.cmm=ALL-UNNAMED VASSAL.launch.ModuleManager "$@"
