#!/usr/bin/env bash

#
# Execute this file to launch VASSAL on Unix
#

set -e

# --- Ensure a locale which can read non-ASCII filenames -------------
# The JVM decodes filenames with the charset named by sun.jnu.encoding,
# which it derives from the C library locale at startup and which cannot
# be changed afterwards with -D. If that locale is unusable -- unset, C /
# POSIX, or naming locale data which is not installed -- the charset is
# ASCII and every path holding a non-ASCII character throws
# InvalidPathException, which is enough to break simply listing a home
# directory (vassalengine/vassal#15314).
#
# This has to come before the version check below, which is itself a JVM
# launch: an install directory holding a non-ASCII character would not be
# found on the classpath either.
#
# Only ASCII is repaired. A locale which is merely not UTF-8 is left
# alone, since a legacy single-byte system round-trips its own filenames
# perfectly well and overriding it would be the regression rather than
# the fix. dist/linux/VASSAL.sh carries the same block and the reasoning
# behind it in full. Set VASSAL_NO_LOCALE_FIX=1 to skip this.
locale_handles_non_ascii() {
  # the locale must load as a whole ...
  [ -z "$(locale 2>&1 >/dev/null)" ] || return 1
  # ... and must not be ASCII, which cannot represent non-ASCII filenames
  case "$(locale charmap 2>/dev/null)" in
    ANSI_X3.4-1968|ANSI_X3.4-1986|US-ASCII|ASCII) return 1 ;;
  esac
  return 0
}

if [ -z "$VASSAL_NO_LOCALE_FIX" ] && ! locale_handles_non_ascii ; then
  for loc in C.UTF-8 C.utf8 en_US.UTF-8 en_US.utf8 ; do
    if LC_ALL=$loc locale_handles_non_ascii ; then
      export LC_ALL=$loc
      break
    fi
  done
fi

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

# Launch VASSSAL
"$JAVA" -Duser.dir="$INSTALL_DIR" -classpath "$INSTALL_DIR"/lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
