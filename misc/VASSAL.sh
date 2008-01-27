#!/bin/sh

# Execute this file to launch VASSAL on MacOS or Linux
cd `dirname "$0"` && java -Xmx512M -cp lib/Vengine.jar VASSAL.launch.Main

#java -Xmx512M -cp 'lib/*' VASSAL.launch.Main

