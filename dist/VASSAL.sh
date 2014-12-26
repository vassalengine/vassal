#!/bin/sh

#
# Execute this file to launch VASSAL on MacOS or Linux
#

# Find where VASSAL is installed, dereferencing symlinks
INSTALL_DIR=$(cd "$(dirname "$0")"; pwd)

# Launch VASSSAL
java -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
