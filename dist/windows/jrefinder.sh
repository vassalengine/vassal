#!/bin/bash

for ((i=68001; $i < 68500; i=$i+1)); do wget --max-redirect=0 "http://javadl.sun.com/webapps/download/AutoDL?BundleId=$i" 2>&1 ; done >jres
