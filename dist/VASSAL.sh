#!/bin/sh

# Execute this file to launch VASSAL on MacOS or Linux
cd $(dirname $(readlink -f "$0")) && java -classpath lib/Vengine.jar VASSAL.launch.ModuleManager "$@"
