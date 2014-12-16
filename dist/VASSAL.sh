#!/bin/sh

#
# Execute this file to launch VASSAL on MacOS or Linux
#

# Find where VASSAL is installed, dereferencing symlinks
INSTALL_DIR=$(dirname "$(readlink "$0" || echo "$0")")

# Launch VASSSAL
java -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
