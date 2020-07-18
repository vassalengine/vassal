#
# This Makefile is intended for use on Linux systems. If you are trying to use
# it elsewhere, you are on your own.
#
# If you want to build Windows releases you will need:
#
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

SHELL:=/bin/bash

SRCDIR:=src/main/java
TESTDIR:=src/test/java
LIBDIR:=target/lib
CLASSDIR:=target/classes
TMPDIR:=tmp
JDOCDIR:=javadoc
DOCDIR:=doc
DISTDIR:=dist
JDKDIR:=jdks

JDOCLINK:=file:///usr/share/javadoc/java/api

VNUM:=3.3.1

GITBRANCH:=$(shell git rev-parse --abbrev-ref HEAD)
GITCOMMIT:=$(shell git rev-parse --short HEAD)
BUILDNUM:=$(shell git rev-list --count $(shell git describe --tags --abbrev=0)..)
GITDESC:=$(shell git describe --tags)

ifeq ($(GITBRANCH), master)
VERSION:=$(GITDESC)
else
VERSION:=$(GITDESC)-$(GITBRANCH)
endif

YEAR:=$(shell date +%Y)

CLASSPATH:=$(CLASSDIR):$(shell echo $(LIBDIR)/*.jar | tr ' ' ':')
JAVAPATH:=/usr/bin

MVN:=./mvnw

JDOC:=$(JAVAPATH)/javadoc
JDEPS:=$(JAVAPATH)/jdeps
JLINK:=$(JAVAPATH)/jlink

NSIS:=makensis
#DMG:=dmg
DMG:=../libdmg-hfsplus/dmg/dmg

LAUNCH4J:=~/java/launch4j/launch4j

compile:
	$(MVN) compile

jar: $(LIBDIR)/Vengine.jar

test:
	$(MVN) test

$(TMPDIR):
	mkdir -p $(TMPDIR)

$(LIBDIR)/Vengine.jar:
	$(MVN) package

$(TMPDIR)/module_deps: $(LIBDIR)/Vengine.jar $(TMPDIR)
	echo -n jdk.crypto.ec, >$@
	$(JDEPS) --ignore-missing-deps --print-module-deps $(LIBDIR)/*.jar | tr -d '\n' >>$@

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
# Mac OS X
#

$(JDKDIR)/macosx_jdk.tar.gz:
	wget 'https://api.adoptopenjdk.net/v3/binary/latest/14/ga/mac/x64/jdk/hotspot/normal/adoptopenjdk?project=jdk' -O $@

.PRECIOUS: $(JDKDIR)/macosx_jdk.tar.gz

$(JDKDIR)/macosx: $(JDKDIR)/macosx_jdk.tar.gz
	mkdir $@
	tar -C $@ --strip-components=1 -xvf $<

.SECONDARY: $(JDKDIR)/macosx

$(TMPDIR)/VASSAL-$(VERSION)-macosx/VASSAL.app: $(LIBDIR)/Vengine.jar $(TMPDIR)/module_deps $(JDKDIR)/macosx
	mkdir -p $@/Contents/{MacOS,Resources}
	cp dist/macosx/{PkgInfo,Info.plist} $@/Contents
	sed -i -e 's/%NUMVERSION%/$(VNUM)/g' \
         -e 's/%YEAR%/$(YEAR)/g' $@/Contents/Info.plist
	cp dist/macosx/VASSAL.sh $@/Contents/MacOS
	$(JLINK) --module-path $(JDKDIR)/macosx/Contents/Home/jmods --no-header-files --no-man-pages --strip-debug --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/Contents/MacOS/jre
	cp dist/macosx/VASSAL.icns $@/Contents/Resources
	cp -a $(LIBDIR) $@/Contents/Resources/Java
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
# Other
#

$(TMPDIR)/VASSAL-$(VERSION)-other/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar
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

$(TMPDIR)/VASSAL-$(VERSION)-linux/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar
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

$(JDKDIR)/win%_jdk.zip:
	wget 'https://api.adoptopenjdk.net/v3/binary/latest/14/ga/windows/x$(*)/jdk/hotspot/normal/adoptopenjdk?project=jdk' -O $@

.PRECIOUS: $(JDKDIR)/win32_jdk.zip $(JDKDIR)/win64_jdk.zip

$(JDKDIR)/win%: $(JDKDIR)/win%_jdk.zip
	mkdir $@
	unzip -d $@ $<
	f=($@/*) && mv $@/*/* $@ && rmdir "$${f[@]}"

.SECONDARY: $(JDKDIR)/win32 $(JDKDIR)/win64

$(TMPDIR)/VASSAL.exe: $(TMPDIR) dist/windows/VASSAL.l4j.xml dist/windows/VASSAL.ico
	cp dist/windows/{VASSAL.l4j.xml,VASSAL.ico} $(TMPDIR)
	sed -i -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $(TMPDIR)/VASSAL.l4j.xml
	$(LAUNCH4J) $(CURDIR)/$(TMPDIR)/VASSAL.l4j.xml

$(TMPDIR)/VASSAL-$(VERSION)-windows-%/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar $(TMPDIR)/VASSAL.exe $(TMPDIR)/module_deps $(JDKDIR)/win%
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
	$(JLINK) --module-path $(JDKDIR)/win$(*)/jmods --no-header-files --no-man-pages --strip-debug --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/jre
	for i in `find $@ -type d` ; do \
		echo SetOutPath \"\$$INSTDIR\\`echo $$i | \
			sed -e 's|$@/\?||' -e 's/\//\\\/g'`\" ; \
		find $$i -maxdepth 1 -type f -printf 'File "%p"\n' ; \
	done >$(TMPDIR)/install_files.inc
	sed -e 's/^SetOutPath/RMDir/' \
			-e 's|^File "$@|Delete "$$INSTDIR|' \
			-e 's/\//\\/g' <$(TMPDIR)/install_files.inc | \
		tac	>$(TMPDIR)/uninstall_files.inc

# prevents make from trying to delete these, as they're intermediate files
.SECONDARY: $(TMPDIR)/VASSAL-$(VERSION)-windows-32/VASSAL-$(VERSION) $(TMPDIR)/VASSAL-$(VERSION)-windows-64/VASSAL-$(VERSION)

$(TMPDIR)/VASSAL-$(VERSION)-windows-%.exe: $(TMPDIR)/VASSAL-$(VERSION)-windows-%/VASSAL-$(VERSION)
	$(NSIS) -NOCD -DVERSION=$(VERSION) -DTMPDIR=$(TMPDIR) dist/windows/nsis/installer$*.nsi

release-linux: $(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2

release-macosx: $(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg

release-windows: $(TMPDIR)/VASSAL-$(VERSION)-windows-32.exe $(TMPDIR)/VASSAL-$(VERSION)-windows-64.exe

release-other: $(TMPDIR)/VASSAL-$(VERSION)-other.zip

release: clean jar release-other release-linux release-windows release-macosx

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar

upload:
	rsync -vP $(TMPDIR)/VASSAL-$(VERSION)-{windows-32.exe,windows-64.exe,macosx.dmg,linux.tar.bz2,other.zip,src.zip} web.sourceforge.net:/home/project-web/vassalengine/htdocs/builds

javadoc:
	$(JDOC) -d $(JDOCDIR) -link $(JDOCLINK) -classpath $(CLASSPATH) -sourcepath $(SRCDIR) -subpackages VASSAL

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(MVN) clean

.PHONY: compile test clean release release-linux release-macosx release-windows release-other clean-release javadoc clean-javadoc jar
