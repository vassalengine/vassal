#
# This Makefile is intended for use on Linux systems. If you are trying to use
# it elsewhere, you are on your own.
#
# If you want to build Windows releases you will need:
#
# Maven    (https://maven.apache.org/)
# Launch4j (http://launch4j.sourceforge.net)
# NSIS     (http://nsis.sourceforge.net)
#
# Also you might possibly need mingw32 for use with launch4j, depending on
# the architecture of your machine.
#
# If your Linux distribution does not package nsis, you can build it. Get
# both the (Windows) binary and source for NSIS and build with scons:
#
# scons SKIPSTUBS=all SKIPPLUGINS=all SKIPUTILS=all SKIPMISC=all \
#       NSIS_CONFIG_CONST_DATA_PATH=no PREFIX=/path/to/extracted/zip \
#       install-compiler
#
# You will need to set the LAUNCH4J and NSIS variables to reflect where you
# have installed them on your system.
#
# If you want to build the Windows ICO file, you will need librsvg2,
# ImageMagick, and the GIMP.
#
# If you want to build the Apple ICNS file, you will need librsvg2 and
# png2icns.
#
# If you want to build Apple DMGs, you will need genisoimage and
# libdmg-hfsplus. The latter can be obtained from:
#
# 	https://github.com/vasi/libdmg-hfsplus.git
#
# For the Windows release, you will need a Windows JDK available for bundling
# with VASSAL. Set WINJMODS to the directory in the Windows JDK where the
# jmods are.
#
# For the Mac release, you will need a Mac JDK available for bundling
# with VASSAL. Set OSXJMODS to the directory in the Mac JDK where the
# jmods are.
#

SHELL:=/bin/bash

SRCDIR:=vassal-app/src/main/java
TESTDIR:=vassal-app/src/test/java
LIBDIR:=vassal-app/target/lib
CLASSDIR:=vassal-app/target/classes
TMPDIR:=tmp
JDOCDIR:=javadoc
DOCDIR:=doc
DISTDIR:=dist

JDOCLINK:=file:///usr/share/javadoc/java/api

WINJMODS:=jdk-win/jmods
OSXJMODS:=jdk-osx/Contents/Home/jmods

VNUM:=3.3.1
GITCOMMIT:=$(shell git rev-parse --short HEAD)
BUILDNUM:=$(shell git rev-list --count $(shell git describe --tags --abbrev=0)..)
VERSION:=$(shell git describe --tags)

#CLASSPATH:=$(CLASSDIR):$(LIBDIR)/*

CLASSPATH:=$(CLASSDIR):$(shell echo $(LIBDIR)/*.jar | tr ' ' ':')
JAVAPATH:=/usr/bin

MVN:=mvn

JDOC:=$(JAVAPATH)/javadoc
JDEPS:=$(JAVAPATH)/jdeps
JLINK:=$(JAVAPATH)/jlink

NSIS:=makensis
#DMG:=dmg
DMG:=../libdmg-hfsplus/dmg/dmg

LAUNCH4J:=~/java/launch4j/launch4j

all: fast-compile

fast-compile:
	$(MVN) compile

jar: $(LIBDIR)/Vengine.jar

test:
	$(MVN) test

#show:
#	echo $(patsubst %,-C $(TMPDIR)/doc %,$(wildcard $(TMPDIR)/doc/*))

$(TMPDIR):
	mkdir -p $(TMPDIR)

$(LIBDIR)/Vengine.jar: all
	$(MVN) package

$(TMPDIR)/VASSAL.exe: fast-compile $(TMPDIR)
	cp dist/windows/{VASSAL.l4j.xml,VASSAL.ico} $(TMPDIR)
	sed -i -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $(TMPDIR)/VASSAL.l4j.xml
	$(LAUNCH4J) $(CURDIR)/$(TMPDIR)/VASSAL.l4j.xml

$(TMPDIR)/module_deps: $(LIBDIR)/Vengine.jar $(TMPDIR)
	$(JDEPS) --ignore-missing-deps --print-module-deps $(LIBDIR)/*.jar >$@

#dist/windows/VASSAL.ico:
#	convert -bordercolor Transparent -border 1x1 src/icons/22x22/VASSAL.png $(TMPDIR)/VASSAL-24.png
#	rsvg -w 48 -h 48 -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-48.png
#	rsvg -w 256 -h 256 -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-256.png
#	Then put the 16x16, 24x24, 32x32, 48x48, and 256x256 into the GIMP as layers
#	and save as an ICO file.

#dist/macosx/VASSAL.icns:
#	for i in 48 128 256 512 ; do \
#		rsvg -w $$i -h $$i -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-$$i.png ; \
#	done
#	png2icns $@ src/icons/16x16/VASSAL.png src/icons/32x32/VASSAL.png $(TMPDIR)/VASSAL-48.png $(TMPDIR)/VASSAL-128.png $(TMPDIR)/VASSAL-256.png $(TMPDIR)/VASSAL-512.png

#
# OS X
#

$(TMPDIR)/VASSAL-$(VERSION)-macosx/VASSAL.app: all $(LIBDIR)/Vengine.jar $(TMPDIR)/module_deps
	mkdir -p $@/Contents/{MacOS,Resources}
	cp dist/macosx/{PkgInfo,Info.plist} $@/Contents
	sed -i -e 's/%BUILDNUM%/$(BUILDNUM)/g' \
         -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $@/Contents/Info.plist
	cp dist/macosx/VASSAL.sh $@/Contents/MacOS
	$(JLINK) --module-path $(OSXJMODS) --no-header-files --no-man-pages --strip-debug --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/Contents/MacOS/jre
	cp dist/macosx/VASSAL.icns $@/Contents/Resources
#	svn export $(LIBDIR) $@/Contents/Resources/Java
	cp -a $(LIBDIR) $@/Contents/Resources/Java
#	svn export $(DOCDIR) $@/Contents/Resources/doc
	cp -a $(DOCDIR) $@/Contents/Resources/doc
	cp -a CHANGES LICENSE README $@/Contents/Resources/doc
	cp -a $(LIBDIR)/Vengine.jar $@/Contents/Resources/Java

$(TMPDIR)/VASSAL-$(VERSION)-macosx: $(TMPDIR)/VASSAL-$(VERSION)-macosx/VASSAL.app
	ln -s /Applications $@/Applications
	cp dist/macosx/.DS_Store $@
	mkdir -p $@/.background
	cp dist/macosx/background.png $@/.background
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.app/Contents/MacOS/VASSAL.sh
	chmod 755 $@/VASSAL.app/Contents/MacOS/jre/bin/{java,keytool}
	chmod 755 $@/VASSAL.app/Contents/MacOS/jre/lib/jspawnhelper

$(TMPDIR)/VASSAL-$(VERSION)-macosx-uncompressed.dmg: $(TMPDIR)/VASSAL-$(VERSION)-macosx
	genisoimage -V VASSAL -D -R -apple -no-pad -o $@ $<

$(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg: $(TMPDIR)/VASSAL-$(VERSION)-macosx-uncompressed.dmg
	$(DMG) dmg $< $@

#
# other
#

$(TMPDIR)/VASSAL-$(VERSION)-other/VASSAL-$(VERSION): all $(LIBDIR)/Vengine.jar $(TMPDIR)/VASSAL.exe
	mkdir -p $@
	cp -a $(DOCDIR) $@/doc
	cp -a CHANGES LICENSE README $@
	cp -a $(LIBDIR) $@/lib
	cp dist/VASSAL.sh dist/windows/VASSAL.bat $@
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.{bat,sh}

$(TMPDIR)/VASSAL-$(VERSION)-other.zip: $(TMPDIR)/VASSAL-$(VERSION)-other/VASSAL-$(VERSION)
	pushd $(TMPDIR)/VASSAL-$(VERSION)-other ; zip -9rv ../../$@ VASSAL-$(VERSION) ; popd

#
# Linux
#

$(TMPDIR)/VASSAL-$(VERSION)-linux/VASSAL-$(VERSION): all $(LIBDIR)/Vengine.jar
	mkdir -p $@
	cp -a $(DOCDIR) $@/doc
	cp -a CHANGES LICENSE README $@
	cp -a $(LIBDIR) $@/lib
	cp dist/linux/VASSAL.sh $@
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.sh

$(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2: $(TMPDIR)/VASSAL-$(VERSION)-linux/VASSAL-$(VERSION)
	tar cjvf $@ -C $(TMPDIR)/VASSAL-$(VERSION)-linux VASSAL-$(VERSION)

#
# Windows
#

$(TMPDIR)/VASSAL-$(VERSION)-windows/VASSAL-$(VERSION): all $(LIBDIR)/Vengine.jar $(TMPDIR)/VASSAL.exe $(TMPDIR)/module_deps
	mkdir -p $@
	cp $(TMPDIR)/VASSAL.exe $@
	cp -a CHANGES $@/CHANGES.txt
	cp -a LICENSE $@/LICENSE.txt
	cp -a README $@/README.txt
	cp -a $(DOCDIR) $@/doc
	cp -a $(LIBDIR) $@/lib
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.exe
	$(JLINK) --module-path $(WINJMODS) --no-header-files --no-man-pages --strip-debug --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/jre
	for i in `find $@ -type d` ; do \
		echo SetOutPath \"\$$INSTDIR\\`echo $$i | \
			sed -e 's|$@/\?||' -e 's/\//\\\/g'`\" ; \
		find $$i -maxdepth 1 -type f -printf 'File "%p"\n' ; \
	done >$(TMPDIR)/install_files.inc
	sed -e 's/^SetOutPath/RMDir/' \
			-e 's|^File "$@|Delete "$$INSTDIR|' \
			-e 's/\//\\/g' <$(TMPDIR)/install_files.inc | \
		tac	>$(TMPDIR)/uninstall_files.inc

$(TMPDIR)/VASSAL-$(VERSION)-windows.exe: $(TMPDIR)/VASSAL-$(VERSION)-windows/VASSAL-$(VERSION)
	$(NSIS) -NOCD -DVERSION=$(VERSION) -DTMPDIR=$(TMPDIR) dist/windows/nsis/installer.nsi

release-linux: $(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2

release-macosx: $(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg

release-windows: $(TMPDIR)/VASSAL-$(VERSION)-windows.exe

release-other: $(TMPDIR)/VASSAL-$(VERSION)-other.zip

release: clean jar test release-other release-linux release-windows release-macosx

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar

upload:
	rsync -vP $(TMPDIR)/VASSAL-$(VERSION)-{windows.exe,macosx.dmg,linux.tar.bz2,other.zip,src.zip} web.sourceforge.net:/home/project-web/vassalengine/htdocs/builds

javadoc:
	$(JDOC) -d $(JDOCDIR) -link $(JDOCLINK) -classpath $(CLASSPATH) -sourcepath $(SRCDIR) -subpackages VASSAL

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(MVN) clean

.PHONY: all fast-compile test clean release release-linux release-macosx release-windows release-other clean-release javadoc clean-javadoc jar
