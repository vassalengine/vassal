#!/bin/sh

# Execute this file to launch VASSAL on MacOS or Linux
cd `dirname "$0"` && java `cat heaps 2>/dev/null || echo '-Xms256M -Xmx512M'` -classpath lib/Vengine.jar VASSAL.launch.Main -edit -extract /docsInfo "$@"
