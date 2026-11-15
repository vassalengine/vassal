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
  echo "Error: $JAVA is too old to run this version of Vassal. Please use Java 17 or later." 2>&1
  exit 1
fi

# Java takes the character set for file names from the locale. Without a
# UTF-8 locale (C or POSIX, as when launched outside a desktop session, or a
# LANG that is not installed), any file name with a character outside ASCII
# makes the file chooser fail. A working UTF-8 locale is left alone.
case "$(locale charmap 2>/dev/null)" in
  UTF-8|utf8|UTF8) ;;
  *) export LC_ALL=C.UTF-8 ;;
esac

# Launch VASSSAL
"$JAVA" -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
