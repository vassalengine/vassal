#!/usr/bin/env bash -e

# get the root directory of the bundle and go there
APP_ROOT="$(dirname "$0")/../.."
cd "$APP_ROOT"

# filter out the -psn_* option added by Finder
ARGS=()
for ARG in "$@"; do
  if [[ "$ARG" != -psn_* ]]; then
    ARGS+=($(printf '%q' "$ARG"))
  fi
done

# determine the correct CPU architecture
if sysctl machdep.cpu.brand_string | grep -q Intel ; then
  ARCH=x86_64
else
  ARCH=arm64
fi

# fire it up
exec arch -$ARCH Contents/MacOS/jre/bin/java -classpath Contents/Resources/Java/Vengine.jar -Xdock:name=VASSAL -Xdock:icon=Contents/Resources/VASSAL.icns VASSAL.launch.ModuleManager "${ARGS[@]}"
