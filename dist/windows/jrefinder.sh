#!/bin/bash

for ((i=76210; $i < 77500; i=$i+1)); do wget --max-redirect=0 "http://javadl.sun.com/webapps/download/AutoDL?BundleId=$i" 2>&1 ; done >jres
