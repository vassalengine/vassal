#!/bin/bash -e

#
# Execute this file to launch VASSAL on Unix
#

# Get java from PATH if not set in environment
JAVA="${JAVA:-$(which java)}"

# Check that java exists
if [ ! -x "$JAVA" ]; then
  echo "Error: $JAVA cannot be run. Please ensure that Java is installed." 2>&1
  exit 1
fi

# Find absolute path where VASSAL is installed
INSTALL_DIR=$(cd "$(dirname "$0")"; pwd)

# Check that java is new enough
if ! "$JAVA" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.JavaVersionChecker 2>/dev/null ; then
  echo "Error: $JAVA is too old to run this version of Vassal. Please use Java 11 or later." 2>&1
  exit 1
fi

# Launch VASSSAL
"$JAVA" -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar --add-exports java.desktop/sun.java2d.cmm=ALL-UNNAMED VASSAL.launch.ModuleManager "$@"
