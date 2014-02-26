#!/usr/bin/env bash -e

# get the root directory of the bundle and go there
APP_ROOT="$(dirname "$0")/../.."
cd "$APP_ROOT"

# find Java
# try Apple's Java 6
JAVA="/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
if [ ! -x "$JAVA" ]; then
  # try Apple's Java 5
  JAVA="/System/Library/Frameworks/JavaVM.framework/Versions/1.5/Commands/java"
  if [ ! -x "$JAVA" ]; then
    # try Oracle's Java 7
    JAVA="/Library/Internet Plug-ins/JavaAppletPlugin.plugin/Contents/Home/bin/java"
    if [ ! -x "$JAVA" ]; then
      # try whatever's on the PATH as a last resort
      JAVA="$(which java)"
      if [ ! -x "$JAVA" ]; then
        # you seem not to have Java
        osascript -e 'tell app "System Events" to display alert "VASSAL requires Java in order to run. Please install Java before starting VASSAL." as critical buttons {"OK"}'
        exit 1
      fi
    fi
  fi
fi

# filter out the -psn_* option added by Finder
ARGS=()
for ARG in "$@"; do
  if [[ "$ARG" != -psn_* ]]; then
    ARGS+=($(printf '%q' "$ARG"))
  fi
done

# fire it up
exec "$JAVA" -classpath Contents/Resources/Java/Vengine.jar -Dapple.awt.graphics.UseQuartz=false -Xdock:name=VASSAL -Xdock:icon=Contents/Resources/VASSAL.icns VASSAL.launch.ModuleManager "${ARGS[@]}"
