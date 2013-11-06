#!/usr/bin/env bash

# get the root directory of the bundle
APP_ROOT="$(dirname "$0")/../.."

#
# find a JRE to use
#

# try Java 7
JAVA="/Library/Internet Plug-ins/JavaAppletPlugin.plugin/Contents/Home/bin/java"
if [ ! -x "$JAVA" ]; then
  # try Java 6
  JAVA="/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands/java"
  if [ ! -x "$JAVA" ]; then
    # try whatever's on the PATH
    JAVA="$(which java)"
    if [ ! -x "$JAVA" ]; then
      # you seem not to have Java
      osascript -e 'tell app "System Events" to display alert "VASSAL requires Java in order to run. Please install Java before starting VASSAL." as critical buttons {"OK"}'
      exit 1
    fi
  fi
fi

# fire it up
"$JAVA" -classpath "$APP_ROOT/Contents/Resources/Java/Vengine.jar" -Dapple.awt.graphics.UseQuartz=false -Xdock:name=VASSAL -Xdock:icon="$APP_ROOT/Contents/Resources/VASSAL.icns" VASSAL.launch.ModuleManager "$@"
