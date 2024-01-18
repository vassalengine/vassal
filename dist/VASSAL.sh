#!/usr/bin/env bash

#
# Execute this file to launch VASSAL on Unix
#

set -e

# Get java from PATH if not set in environment
JAVA="${JAVA:-$(which java 2>/dev/null || : )}"

# Check that java exists
if [ ! -x "$JAVA" ]; then
  echo "Error: $JAVA cannot be run. Please ensure that Java is installed." 2>&1
  exit 1
fi

# Dereference any possible symbolic link to executable script, then find
# absolute path where VASSAL is installed 
EXEC_PATH=$(realpath "$0")
INSTALL_DIR=$(dirname "${EXEC_PATH}")

# Check that java is new enough
if ! "$JAVA" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.JavaVersionChecker 2>/dev/null ; then
  echo "Error: $JAVA is too old to run this version of Vassal. Please use Java 11 or later." 2>&1
  exit 1
fi

# Launch VASSSAL
"$JAVA" -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
