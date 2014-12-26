#
# $Id$
#
# This Makefile is intended for use on Linux systems. If you are trying to use
# it elsewhere, you are on your own.
#
# Before building, you will need to install (at least) the following packages:
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

SRCDIR:=src
TESTDIR:=test
LIBDIR:=lib
LIBDIRND:=lib-nondist
CLASSDIR:=bin
TMPDIR:=tmp
JDOCDIR:=javadoc
DOCDIR:=doc
DISTDIR:=dist

VNUM:=3.2.15
#SVNVERSION:=$(shell svnversion | perl -pe 's/(\d+:)?(\d+[MS]?)/$$2/; s/(\d+)M/$$1+1/e')
SVNVERSION:=$(shell git svn log -1 --oneline | grep -oP '^r\K\d+')
#VERSION:=$(VNUM)-svn$(SVNVERSION)
VERSION:=$(VNUM)

#CLASSPATH:=$(CLASSDIR):$(LIBDIR)/*

CLASSPATH:=$(CLASSDIR):$(shell echo $(LIBDIR)/*.jar | tr ' ' ':'):$(shell echo $(LIBDIRND)/*.jar | tr ' ' ':')
JAVAPATH:=/usr/bin

JC:=$(JAVAPATH)/javac
JCFLAGS:=-d $(CLASSDIR) -source 5 -target 5 -Xlint -classpath $(CLASSPATH) \
				 -sourcepath $(SRCDIR)

JAR:=$(JAVAPATH)/jar
JDOC:=$(JAVAPATH)/javadoc
JAVA:=$(JAVAPATH)/java

NSIS:=makensis
#DMG:=dmg
DMG:=../libdmg-hfsplus/dmg/dmg

LAUNCH4J:=~/java/launch4j/launch4j

#SOURCES:=$(shell find $(SRCDIR) -name '*.java' | sed "s/^$(SRCDIR)\///")
#CLASSES:=$(SOURCES:.java=.class)

vpath %.class $(shell find $(CLASSDIR) -type d)
vpath %.java  $(shell find $(SRCDIR) -type d -name .svn -prune -o -print)

#all: $(CLASSDIR) $(CLASSES) i18n icons images help
all: $(CLASSDIR) fast-compile i18n bsh icons images help

$(CLASSDIR):
	mkdir -p $(CLASSDIR)

%.class: %.java
	$(JC) $(JCFLAGS) $<

images: $(CLASSDIR)/images

$(CLASSDIR)/images: $(CLASSDIR)
#	svn export --force $(SRCDIR)/images $(CLASSDIR)/images
	cp -a $(SRCDIR)/images $(CLASSDIR)/images

icons: $(CLASSDIR)/icons

$(CLASSDIR)/icons: $(CLASSDIR)
#	svn export --force $(SRCDIR)/icons $(CLASSDIR)/icons
	cp -a $(SRCDIR)/icons $(CLASSDIR)/icons

help: $(CLASSDIR)/help

$(CLASSDIR)/help: $(CLASSDIR)
#	svn export --force $(SRCDIR)/help $(CLASSDIR)/help
	cp -a $(SRCDIR)/help $(CLASSDIR)/help

i18n: $(CLASSDIR)
	for i in `cd $(SRCDIR) && find VASSAL -name '*.properties'`; do cp $(SRCDIR)/$$i $(CLASSDIR)/$$i; done

bsh: $(CLASSDIR)
	for i in `cd $(SRCDIR) && find VASSAL -name '*.bsh'`; do cp $(SRCDIR)/$$i $(CLASSDIR)/$$i; done

fast-compile: version $(CLASSDIR)
	$(JC) $(JCFLAGS) $(shell find $(SRCDIR) -name '*.java')

jar: $(LIBDIR)/Vengine.jar

test:
	$(JC) $(JCFLAGS) $(shell find $(TESTDIR) -name '*.java')
	$(JAVA) -classpath $(CLASSPATH) org.junit.runner.JUnitCore $(shell grep -l '@Test' `find $(TESTDIR) -name '*.java'` | sed "s/^$(TESTDIR)\/\(.*\)\.java$$/\1/" | tr '/' '.')

#show:
#	echo $(patsubst %,-C $(TMPDIR)/doc %,$(wildcard $(TMPDIR)/doc/*)) 

$(TMPDIR):
	mkdir -p $(TMPDIR)

$(LIBDIR)/Vengine.jar: all $(TMPDIR)
	cp dist/Vengine.mf $(TMPDIR)
	(echo -n 'Class-Path: ' ; \
		find $(LIBDIR) -name '*.jar' -printf '%f\n  ' | \
		sed -e '/Vengine.jar/d' -e '/^  $$/d' \
	) >>$(TMPDIR)/Vengine.mf
	$(JAR) cvfm $@ $(TMPDIR)/Vengine.mf -C $(CLASSDIR) . -C $(SRCDIR) logback.xml
	cd $(LIBDIR) ; $(JAR) i $(@F) ; cd ..
	chmod 664 $@

$(TMPDIR)/VASSAL.exe: Info.class $(TMPDIR)
	cp dist/windows/{VASSAL.l4j.xml,VASSAL.ico} $(TMPDIR)
	sed -i -e 's/%SVNVERSION%/$(SVNVERSION)/g' \
         -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $(TMPDIR)/VASSAL.l4j.xml
	$(LAUNCH4J) $(CURDIR)/$(TMPDIR)/VASSAL.l4j.xml

version:
	sed -ri 's/ VERSION = ".*"/ VERSION = "$(VERSION)"/' $(SRCDIR)/VASSAL/Info.java

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

$(TMPDIR)/VASSAL-$(VERSION)-macosx/VASSAL.app: all $(LIBDIR)/Vengine.jar $(TMPDIR)
	mkdir -p $@/Contents/{MacOS,Resources}
	cp dist/macosx/{PkgInfo,Info.plist} $@/Contents
	sed -i -e 's/%SVNVERSION%/$(SVNVERSION)/g' \
         -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $@/Contents/Info.plist
	cp dist/macosx/VASSAL.sh $@/Contents/MacOS
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

$(TMPDIR)/VASSAL-$(VERSION)-macosx-uncompressed.dmg: $(TMPDIR)/VASSAL-$(VERSION)-macosx
	genisoimage -V VASSAL -D -R -apple -no-pad -o $@ $<
	
$(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg: $(TMPDIR)/VASSAL-$(VERSION)-macosx-uncompressed.dmg
	$(DMG) dmg $< $@

$(TMPDIR)/VASSAL-$(VERSION)-other.zip: all $(LIBDIR)/Vengine.jar $(TMPDIR)/VASSAL.exe
	mkdir -p $(TMPDIR)/VASSAL-$(VERSION)
#	svn export $(DOCDIR) $(TMPDIR)/VASSAL-$(VERSION)/doc
	cp -a $(DOCDIR) $(TMPDIR)/VASSAL-$(VERSION)/doc
	cp -a CHANGES LICENSE README $(TMPDIR)/VASSAL-$(VERSION)
#	svn export $(LIBDIR) $(TMPDIR)/VASSAL-$(VERSION)/lib
	cp -a $(LIBDIR) $(TMPDIR)/VASSAL-$(VERSION)/lib
	cp dist/VASSAL.sh dist/windows/VASSAL.bat $(TMPDIR)/VASSAL.exe $(TMPDIR)/VASSAL-$(VERSION)
	find $(TMPDIR)/VASSAL-$(VERSION) -type f -exec chmod 644 \{\} \+
	find $(TMPDIR)/VASSAL-$(VERSION) -type d -exec chmod 755 \{\} \+
	chmod 755 $(TMPDIR)/VASSAL-$(VERSION)/VASSAL.{bat,exe,sh}
	cd $(TMPDIR) ; zip -9rv $(@F) VASSAL-$(VERSION) ; cd ..

$(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2: release-other
	cp dist/VASSAL.sh $(TMPDIR)/VASSAL-$(VERSION)
	-rm $(TMPDIR)/VASSAL-$(VERSION)/VASSAL.{bat,exe}
	tar cjvf $@ -C $(TMPDIR) VASSAL-$(VERSION)

$(TMPDIR)/VASSAL-$(VERSION)-windows.exe: release-other $(TMPDIR)/VASSAL.exe
	-rm $(TMPDIR)/VASSAL-$(VERSION)/VASSAL.{sh,bat}
	cp $(TMPDIR)/VASSAL.exe $(TMPDIR)/VASSAL-$(VERSION)
	mv $(TMPDIR)/VASSAL-$(VERSION)/CHANGES $(TMPDIR)/VASSAL-$(VERSION)/CHANGES.txt
	mv $(TMPDIR)/VASSAL-$(VERSION)/LICENSE $(TMPDIR)/VASSAL-$(VERSION)/LICENSE.txt
	mv $(TMPDIR)/VASSAL-$(VERSION)/README $(TMPDIR)/VASSAL-$(VERSION)/README.txt
	for i in `find $(TMPDIR)/VASSAL-$(VERSION) -type d` ; do \
		echo SetOutPath \"\$$INSTDIR\\`echo $$i | \
			sed -e 's/tmp\/VASSAL-$(VERSION)\/\?//' -e 's/\//\\\/g'`\" ; \
		find $$i -maxdepth 1 -type f -printf 'File "%p"\n' ; \
	done >$(TMPDIR)/install_files.inc
	sed -e 's/^SetOutPath/RMDir/' \
			-e 's/^File "$(TMPDIR)\/VASSAL-$(VERSION)/Delete "$$INSTDIR/' \
			-e 's/\//\\/g' <$(TMPDIR)/install_files.inc | \
		tac	>$(TMPDIR)/uninstall_files.inc
	$(NSIS) -NOCD -DVERSION=$(VERSION) -DTMPDIR=$(TMPDIR) dist/windows/nsis/installer.nsi

$(TMPDIR)/VASSAL-$(VERSION)-src.zip: version
#	svn export . $(TMPDIR)/VASSAL-$(VERSION)-src
	git checkout-index -a -f --prefix=$(TMPDIR)/VASSAL-$(VERSION)-src/
	cd $(TMPDIR) ; zip -9rv $(@F) VASSAL-$(VERSION)-src ; cd ..

release-linux: $(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2

release-macosx: $(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg

release-windows: $(TMPDIR)/VASSAL-$(VERSION)-windows.exe

release-other: $(TMPDIR)/VASSAL-$(VERSION)-other.zip

release-src: $(TMPDIR)/VASSAL-$(VERSION)-src.zip

release: clean jar test release-other release-linux release-windows release-macosx

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar

upload:
	rsync -vP $(TMPDIR)/VASSAL-$(VERSION)-{windows.exe,macosx.dmg,linux.tar.bz2,other.zip,src.zip} web.sourceforge.net:/home/project-web/vassalengine/htdocs/builds

javadoc:
	$(JDOC) -d $(JDOCDIR) -link http://java.sun.com/javase/6/docs/api -sourcepath $(SRCDIR) -subpackages VASSAL 

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(RM) -r $(CLASSDIR)/*

.PHONY: all bsh fast-compile test clean release release-linux release-macosx release-windows release-other clean-release i18n icons images help javadoc clean-javadoc version jar
