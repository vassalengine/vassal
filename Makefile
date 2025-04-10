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
# If you want to build Mac or Windows packages, you will need JDKs for jlink
# to use during packaging. For Macs, you will additionally need genisoimage,
# and dmg from libdmg-hfsplus.
#
# For producing the release announcements from the templates, you will need
# jinja2 and jinja2-cli.
#
# Run bootstrap.sh to download the appropriate JDKs and download and compile
# dmg.
#

SHELL:=/bin/bash

LIBDIR:=release-prepare/target/lib
TMPDIR:=tmp
DOCDIR:=release-prepare/target/doc
DISTDIR:=dist
JDKDIR:=$(DISTDIR)/jdks
JDOCDIR:=jdoc

# numeric part of the version only
VNUM:=3.8.0
# major-minor part of the version
V_MAJ_MIN:=$(shell echo "$(VNUM)" | cut -f1,2 -d'.')

MAVEN_VERSION:=$(VNUM)-SNAPSHOT
#MAVEN_VERSION:=$(VNUM)-beta5
#MAVEN_VERSION:=$(VNUM)

JARNAME:=vassal-app-$(MAVEN_VERSION)

#Slashes are substituted so as not to create subdirectories
GITBRANCH := $(subst /,_,$(shell git rev-parse --abbrev-ref HEAD))
GITCOMMIT:=$(shell git rev-parse --short HEAD)

ifeq ($(shell git describe --tags), $(MAVEN_VERSION))
  # we are at a release tag
  VERSION:=$(MAVEN_VERSION)
else ifeq ($(patsubst release-%,release,$(GITBRANCH)), release)
  # we are on a release branch
  VERSION:=$(MAVEN_VERSION)-$(GITCOMMIT)
else
  # we are on a branch
  VERSION:=$(MAVEN_VERSION)-$(GITCOMMIT)-$(GITBRANCH)
endif

YEAR:=$(shell date +%Y)
VERSION_50:=$(shell echo "$(VERSION)" | cut -c1-50)

MVN:=./mvnw

DMG:=$(DISTDIR)/dmg/libdmg-hfsplus/build/dmg/dmg
LIPO:=$(DISTDIR)/lipo/lipo_linux_amd64

NSIS:=makensis
LAUNCH4J:=$(DISTDIR)/launch4j/launch4j/launch4j

SKIPS:=

# -Dasciidoctor.skip=true
# -Dspotbugs.skip=true
# -Dlicense.skipDownloadLicenses
# -Dclirr.skip=true
# -Dmaven.javadoc.skip=true
# -Dpmd.skip=true
# -Dmaven.test.skip=true

jar: SKIPS:=-Dasciidoctor.skip=true -Dspotbugs.skip=true -Dlicense.skipDownloadLicenses
jar: $(LIBDIR)/Vengine.jar

compile:
	$(MVN) compile

version-set:
	$(MVN) versions:set -DnewVersion=$(MAVEN_VERSION) -DgenerateBackupPoms=false

test:
	$(MVN) test

$(TMPDIR) $(JDOCDIR):
	mkdir -p $@

$(LIBDIR)/Vengine.jar: version-set
	$(MVN) deploy -DgitVersion=$(VERSION) -Dasciidoctor.attributes=optimize $(SKIPS)
	mv $(LIBDIR)/$(JARNAME).jar $@

$(TMPDIR)/module_deps: $(LIBDIR)/Vengine.jar | $(TMPDIR)
	echo -n jdk.crypto.ec, >$@
	jdeps --ignore-missing-deps --print-module-deps --multi-release 11 $(LIBDIR)/*.jar | tr -d '\n' >>$@

#$(DISTDIR)/windows/VASSAL.ico:
#	convert -bordercolor Transparent -border 1x1 src/icons/22x22/VASSAL.png $(TMPDIR)/VASSAL-24.png
#	rsvg -w 48 -h 48 -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-48.png
#	rsvg -w 256 -h 256 -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-256.png
#	Then put the 16x16, 24x24, 32x32, 48x48, and 256x256 into the GIMP as layers
#	and save as an ICO file.

#$(DISTDIR)/macos/VASSAL.icns:
#	for i in 48 128 256 512 ; do \
#		rsvg -w $$i -h $$i -f png src/icons/scalable/VASSAL.svg $(TMPDIR)/VASSAL-$$i.png ; \
#	done
#	png2icns $@ src/icons/16x16/VASSAL.png src/icons/32x32/VASSAL.png $(TMPDIR)/VASSAL-48.png $(TMPDIR)/VASSAL-128.png $(TMPDIR)/VASSAL-256.png $(TMPDIR)/VASSAL-512.png

#
# MacOS
#

$(TMPDIR)/macos-%-$(VERSION)-build/VASSAL.app: $(LIBDIR)/Vengine.jar $(TMPDIR)/module_deps $(JDKDIR)/macos-%
	mkdir -p $@/Contents/{MacOS,Resources}
	cp $(DISTDIR)/macos/{PkgInfo,Info.plist} $@/Contents
	sed -i -e 's/%NUMVERSION%/$(VNUM)/g' \
         -e 's/%YEAR%/$(YEAR)/g' $@/Contents/Info.plist
	cp $(DISTDIR)/macos/VASSAL.sh $@/Contents/MacOS
	jlink --module-path $(JDKDIR)/macos-$(*)/Contents/Home/jmods --no-header-files --no-man-pages --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/Contents/MacOS/jre
	cp $(DISTDIR)/macos/VASSAL.icns $@/Contents/Resources
	cp -a $(LIBDIR) $@/Contents/Resources/Java
	cp -a $(DOCDIR) $@/Contents/Resources/doc
	cp -a CHANGES LICENSE README.md $@/Contents/Resources/doc
	cp -a $(LIBDIR)/Vengine.jar $@/Contents/Resources/Java

$(TMPDIR)/macos-universal-$(VERSION)-build/VASSAL.app: $(TMPDIR)/macos-x86_64-$(VERSION)-build/VASSAL.app $(TMPDIR)/macos-aarch64-$(VERSION)-build/VASSAL.app
	mkdir -p $@
	cp -av $(TMPDIR)/macos-aarch64-$(VERSION)-build/VASSAL.app/* $@
	find $@ -type f | while read f ; do \
		if file "$${f/universal/aarch64}" | grep 'Mach-O.\+arm64' ; then \
			echo "$$f" ; \
			$(LIPO) -create -output "$$f" "$${f/universal/x86_64}" "$${f/universal/aarch64}" ; \
		fi ; \
	done

$(TMPDIR)/macos-%-$(VERSION)-build: $(TMPDIR)/macos-%-$(VERSION)-build/VASSAL.app
	ln -s /Applications $@/Applications
	cp $(DISTDIR)/macos/.DS_Store $@
	mkdir -p $@/.background
	cp $(DISTDIR)/macos/background.png $@/.background
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.app/Contents/MacOS/VASSAL.sh
	chmod 755 $@/VASSAL.app/Contents/MacOS/jre/bin/{java,keytool}
	file $@/VASSAL.app/Contents/MacOS/jre/bin/java
	chmod 755 $@/VASSAL.app/Contents/MacOS/jre/lib/jspawnhelper

$(TMPDIR)/VASSAL-$(VERSION)-macos-%-uncompressed.dmg: $(TMPDIR)/macos-%-$(VERSION)-build
	genisoimage -V VASSAL -D -R -apple -no-pad -o $@ $<

$(TMPDIR)/VASSAL-$(VERSION)-macos-%.dmg: $(TMPDIR)/VASSAL-$(VERSION)-macos-%-uncompressed.dmg
	$(DMG) $< $@

#
# Other
#

$(TMPDIR)/other-$(VERSION)-build/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar
	mkdir -p $@
	cp -a $(DOCDIR) $@/doc
	cp -a CHANGES LICENSE README.md $@
	cp -a $(LIBDIR) $@/lib
	cp $(DISTDIR)/VASSAL.sh $(DISTDIR)/windows/VASSAL.bat $@
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.{bat,sh}

$(TMPDIR)/VASSAL-$(VERSION)-other.zip: $(TMPDIR)/other-$(VERSION)-build/VASSAL-$(VERSION)
	pushd $(TMPDIR)/other-$(VERSION)-build ; zip -9rv ../../$@ VASSAL-$(VERSION) ; popd

#
# Linux
#

$(TMPDIR)/linux-$(VERSION)-build/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar
	mkdir -p $@
	cp -a $(DOCDIR) $@/doc
	cp -a CHANGES LICENSE README.md $@
	cp -a $(LIBDIR) $@/lib
	cp $(DISTDIR)/VASSAL.sh $@
	cp vassal-app/src/main/resources/icons/scalable/VASSAL.svg $@
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.sh

$(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2: $(TMPDIR)/linux-$(VERSION)-build/VASSAL-$(VERSION)
	tar cjvf $@ -C $(TMPDIR)/linux-$(VERSION)-build VASSAL-$(VERSION)

#
# Windows
#

$(TMPDIR)/windows-%-$(VERSION)-build:
	mkdir -p $@

JREOPTS:=

$(TMPDIR)/windows-noinst-$(VERSION)-build/VASSAL.l4j.xml: JREOPTS:=<opt>-DVASSAL.conf="%EXEDIR%\\..\\VASSAL"</opt>

$(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.l4j.xml: $(DISTDIR)/windows/VASSAL.l4j.xml.in | $(TMPDIR)/windows-%-$(VERSION)-build
	sed -e 's/%NUMVERSION%/$(VNUM)/g' \
			-e 's/%FULLVERSION%/$(VERSION_50)/g' \
			-e 's|%JREOPTS%|$(JREOPTS)|g' $< >$@

$(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.ico: $(DISTDIR)/windows/VASSAL.ico | $(TMPDIR)/windows-%-$(VERSION)-build
	cp $< $@

$(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.exe: $(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.l4j.xml $(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.ico
	$(LAUNCH4J) $(CURDIR)/$<

$(TMPDIR)/windows-%-$(VERSION)-build/VASSAL-$(VERSION): $(LIBDIR)/Vengine.jar $(TMPDIR)/windows-%-$(VERSION)-build/VASSAL.exe $(TMPDIR)/module_deps $(JDKDIR)/windows-%
	mkdir -p $@
	mv $(TMPDIR)/windows-$(*)-$(VERSION)-build/VASSAL.exe $@
	cp -a CHANGES $@/CHANGES.txt
	cp -a LICENSE $@/LICENSE.txt
	cp -a README.md $@
	cp -a $(DOCDIR) $@/doc
	cp -a $(LIBDIR) $@/lib
	find $@ -type f -exec chmod 644 \{\} \+
	find $@ -type d -exec chmod 755 \{\} \+
	chmod 755 $@/VASSAL.exe
	jlink --module-path $(JDKDIR)/windows-$(*)/jmods --no-header-files --no-man-pages --add-modules $(file < $(TMPDIR)/module_deps) --compress=2 --output $@/jre

$(TMPDIR)/windows-noinst-$(VERSION)-build/VASSAL-$(VERSION): $(TMPDIR)/windows-64-$(VERSION)-build/VASSAL-$(VERSION) $(TMPDIR)/windows-noinst-$(VERSION)-build/VASSAL.exe
	cp -a $< $@
	mv $(TMPDIR)/windows-noinst-$(VERSION)-build/VASSAL.exe $@

$(TMPDIR)/windows-%-$(VERSION)-build/install_files.inc: $(TMPDIR)/windows-%-$(VERSION)-build/VASSAL-$(VERSION)
	for i in `find $< -type d` ; do \
		echo SetOutPath \"\$$INSTDIR\\`echo $$i | \
			sed -e 's|$</\?||' -e 's/\//\\\/g'`\" ; \
		find $$i -maxdepth 1 -type f -printf 'File "%p"\n' ; \
	done >$@

$(TMPDIR)/windows-%-$(VERSION)-build/uninstall_files.inc: $(TMPDIR)/windows-%-$(VERSION)-build/install_files.inc
	sed -e 's/^SetOutPath/RMDir/' \
			-e 's|^File "$(TMPDIR)/windows-$(*)-$(VERSION)-build/VASSAL-$(VERSION)|Delete "$$INSTDIR|' \
			-e 's/\//\\/g' <$< | \
		tac	>$@

$(TMPDIR)/VASSAL-$(VERSION)-windows-x86_32.exe: BITS:=32
$(TMPDIR)/VASSAL-$(VERSION)-windows-x86_64.exe: BITS:=64
$(TMPDIR)/VASSAL-$(VERSION)-windows-aarch64.exe: BITS:=64

$(TMPDIR)/VASSAL-$(VERSION)-windows-%.exe: $(TMPDIR)/windows-%-$(VERSION)-build/VASSAL-$(VERSION) $(TMPDIR)/windows-%-$(VERSION)-build/install_files.inc $(TMPDIR)/windows-%-$(VERSION)-build/uninstall_files.inc
	$(NSIS) -NOCD -DVERSION=$(VERSION) -DNUMVERSION=$(VNUM) -DTMPDIR=$(TMPDIR) -DARCH=$* -DBITS=$(BITS) $(DISTDIR)/windows/nsis/installer.nsi

$(TMPDIR)/VASSAL-$(VERSION)-windows-noinst.zip: $(TMPDIR)/windows-noinst-$(VERSION)-build/VASSAL-$(VERSION)
	pushd $(TMPDIR)/windows-noinst-$(VERSION)-build ; zip -9rv ../../$@ VASSAL-$(VERSION) ; popd

#
# Packages
#

release-linux: $(TMPDIR)/VASSAL-$(VERSION)-linux.tar.bz2

release-macos: release-macos-universal

release-macos-universal: $(TMPDIR)/VASSAL-$(VERSION)-macos-universal.dmg

release-windows: release-windows-x86_64 release-windows-aarch64

release-windows-x86_32: $(TMPDIR)/VASSAL-$(VERSION)-windows-x86_32.exe

release-windows-x86_64: $(TMPDIR)/VASSAL-$(VERSION)-windows-x86_64.exe

release-windows-aarch64: $(TMPDIR)/VASSAL-$(VERSION)-windows-aarch64.exe

release-windows-noinst: $(TMPDIR)/VASSAL-$(VERSION)-windows-noinst.zip

release-other: $(TMPDIR)/VASSAL-$(VERSION)-other.zip

$(TMPDIR)/VASSAL-$(VERSION).sha256:
	pushd $(TMPDIR) ; sha256sum VASSAL-$(VERSION)-linux.tar.bz2 VASSAL-$(VERSION)-macos-universal.dmg VASSAL-$(VERSION)-windows-x86_32.exe VASSAL-$(VERSION)-windows-x86_64.exe VASSAL-$(VERSION)-windows-aarch64.exe VASSAL-$(VERSION)-other.zip >$(@F) ; popd

release-sha256: $(TMPDIR)/VASSAL-$(VERSION).sha256

$(TMPDIR)/notes.json: $(DISTDIR)/notes/data.json | $(TMPDIR)
	jinja2 --strict -Dversion=$(VERSION) -Dversion_feature=$(V_MAJ_MIN) -o $@ $^

$(TMPDIR)/NOTES-%: $(DISTDIR)/notes/NOTES-%.jinja $(TMPDIR)/notes.json | $(TMPDIR)
	jinja2 --strict -Dversion=$(VERSION) -Dversion_feature=$(V_MAJ_MIN) -o $@ $^

release-announcements: $(TMPDIR)/NOTES-bgg $(TMPDIR)/NOTES-csw $(TMPDIR)/NOTES-news $(TMPDIR)/NOTES-fb $(TMPDIR)/NOTES-gh

release: clean release-other release-linux release-windows release-macos

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar

post-release: version-set

vassal-app/target/$(JARNAME)-javadoc.jar: $(LIBDIR)/Vengine.jar

javadoc: vassal-app/target/$(JARNAME)-javadoc.jar | $(JDOCDIR)
	pushd $(JDOCDIR) ; unzip ../vassal-app/target/$(JARNAME)-javadoc.jar ; popd

version-print:
	@echo $(VERSION)

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(MVN) clean

# prevents make from trying to delete intermediate files
.SECONDARY:

.PHONY: compile test clean release release-linux release-macos release-macos-universal release-windows release-windows-x86_32 release-windows-x86_64 release-windows-aarch64 release-other release-sha256 release-announcements clean-release post-release javadoc jar clean-javadoc version-set version-print
