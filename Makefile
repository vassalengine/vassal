SHELL:=/bin/bash

SRCDIR:=src
LIBDIR:=lib
CLASSDIR:=classes
TMPDIR:=tmp
JDOCDIR:=javadoc
DOCDIR:=doc
DISTDIR:=dist

VNUM:=3.1.0
SVNVERSION:=$(shell svnversion | perl -pe 's/(\d+:)?(\d+[MS]?)/$$2/; s/(\d+)M/$$1+1/e')
VERSION:=$(VNUM)-svn$(SVNVERSION)
#VERSION:=$(VNUM)-beta5

#CLASSPATH:=$(CLASSDIR):$(LIBDIR)/*

CLASSPATH:=$(CLASSDIR):$(shell echo $(LIBDIR)/*.jar | tr ' ' ':')
JAVAPATH:=/usr/bin

JC:=$(JAVAPATH)/javac
JCFLAGS:=-d $(CLASSDIR) -source 5 -target 5 -Xlint -classpath $(CLASSPATH) \
				 -sourcepath $(SRCDIR)

JAR:=$(JAVAPATH)/jar
JDOC:=$(JAVAPATH)/javadoc

NSIS:=PATH=$$PATH:~/java/nsis/bin makensis

LAUNCH4J:=~/java/launch4j/launch4j

SOURCES:=$(shell find $(SRCDIR) -name '*.java' | sed "s/^$(SRCDIR)\///")
CLASSES:=$(SOURCES:.java=.class)
JARS:=Vengine.jar

vpath %.class $(shell find $(CLASSDIR) -type d)
vpath %.java  $(shell find $(SRCDIR) -type d -name .svn -prune -o -print)
vpath %.jar $(LIBDIR)

#all: $(CLASSDIR) $(CLASSES) i18n images help
all: $(CLASSDIR) fast-compile i18n images help

$(CLASSDIR):
	mkdir -p $(CLASSDIR)

%.class: %.java
	$(JC) $(JCFLAGS) $<

images: $(CLASSDIR)/images

$(CLASSDIR)/images: $(CLASSDIR)
	svn export --force $(SRCDIR)/images $(CLASSDIR)/images

help: $(CLASSDIR)/help

$(CLASSDIR)/help: $(CLASSDIR)
	svn export --force $(SRCDIR)/help $(CLASSDIR)/help

i18n: $(CLASSDIR)
	for i in `cd $(SRCDIR) && find VASSAL -name '*.properties'`; do cp $(SRCDIR)/$$i $(CLASSDIR)/$$i; done

fast: clean $(CLASSDIR) fast-compile i18n images help

fast-compile:
	$(JC) $(JCFLAGS) $(shell find $(SRCDIR) -name '*.java')

#show:
#	echo $(patsubst %,-C $(TMPDIR)/doc %,$(wildcard $(TMPDIR)/doc/*)) 

$(TMPDIR):
	mkdir -p $(TMPDIR)

Vengine.jar: all $(TMPDIR)
	cp dist/Vengine.mf $(TMPDIR)
	(echo -n 'Class-Path: ' ; \
		find $(LIBDIR) -name '*.jar' -printf '%f\n  ' | \
		sed -e '/Vengine.jar/d' -e '/AppleJavaExtensions.jar/d' -e '/^  $$/d' \
	) >>$(TMPDIR)/Vengine.mf
	$(JAR) cvfm $(LIBDIR)/$@ $(TMPDIR)/Vengine.mf -C $(CLASSDIR) .
	cd $(LIBDIR) ; $(JAR) i $@ ; cd ..

$(TMPDIR)/VASSAL.exe: Info.class $(TMPDIR)
	cp dist/windows/VASSAL.l4j.xml $(TMPDIR)
	sed -i -e 's/%SVNVERSION%/$(SVNVERSION)/g' \
         -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $(TMPDIR)/VASSAL.l4j.xml
	$(LAUNCH4J) $(CURDIR)/$(TMPDIR)/VASSAL.l4j.xml

version:
	sed -ri 's/VERSION = ".*"/VERSION = "$(VERSION)"/' $(SRCDIR)/VASSAL/Info.java

$(TMPDIR)/VASSAL-$(VERSION).app: version all $(JARS) $(TMPDIR)
	mkdir -p $@/Contents/{MacOS,Resources}
	cp dist/macosx/{PkgInfo,Info.plist} $@/Contents
	sed -i -e 's/%SVNVERSION%/$(SVNVERSION)/g' \
         -e 's/%NUMVERSION%/$(VNUM)/g' \
				 -e 's/%FULLVERSION%/$(VERSION)/g' $@/Contents/Info.plist
	cp dist/macosx/JavaApplicationStub $@/Contents/MacOS
	cp dist/macosx/VASSAL.icns $@/Contents/Resources
	svn export $(LIBDIR) $@/Contents/Resources/Java
	rm $@/Contents/Resources/Java/AppleJavaExtensions.jar
	svn export $(DOCDIR) $@/Contents/Resources/doc
	cp $(LIBDIR)/Vengine.jar $@/Contents/Resources/Java

$(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg: $(TMPDIR)/VASSAL-$(VERSION).app
	genisoimage -V VASSAL-$(VERSION) -r -apple -root VASSAL-$(VERSION).app -o $@ $<

$(TMPDIR)/VASSAL-$(VERSION)-generic.zip: version all $(JARS)
	mkdir -p $(TMPDIR)/VASSAL-$(VERSION)
	svn export $(DOCDIR) $(TMPDIR)/VASSAL-$(VERSION)/doc
	svn export $(LIBDIR) $(TMPDIR)/VASSAL-$(VERSION)/lib
	rm $(TMPDIR)/VASSAL-$(VERSION)/lib/AppleJavaExtensions.jar
	cp $(LIBDIR)/Vengine.jar $(TMPDIR)/VASSAL-$(VERSION)/lib
	cp dist/VASSAL.sh dist/windows/VASSAL.bat $(TMPDIR)/VASSAL-$(VERSION)
	cd $(TMPDIR) ; zip -9rv $(notdir $@) VASSAL-$(VERSION) ; cd ..

$(TMPDIR)/VASSAL-$(VERSION)-windows.exe: version release-generic $(TMPDIR)/VASSAL.exe
	rm $(TMPDIR)/VASSAL-$(VERSION)/VASSAL.sh
	cp $(TMPDIR)/VASSAL.exe $(TMPDIR)/VASSAL-$(VERSION)
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
	svn export . $(TMPDIR)/VASSAL-$(VERSION)-src
	cd $(TMPDIR) ; zip -9rv $(notdir $@) VASSAL-$(VERSION)-src ; cd ..

release-macosx: $(TMPDIR)/VASSAL-$(VERSION)-macosx.dmg

release-windows: $(TMPDIR)/VASSAL-$(VERSION)-windows.exe

release-generic: $(TMPDIR)/VASSAL-$(VERSION)-generic.zip

release-src: $(TMPDIR)/VASSAL-$(VERSION)-src.zip

release: clean release-generic release-windows release-macosx

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar

#upload:
#	scp $(TMPDIR)/VASSAL-$(VERSION){-windows.exe,-macosx.dmg,.zip} nomic.net:www/tmp/vassal

javadoc:
	$(JDOC) -d $(JDOCDIR) -link http://java.sun.com/javase/6/docs/api -sourcepath $(SRCDIR) -subpackages VASSAL 

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(RM) -r $(CLASSDIR)/*

.PHONY: all fast fast-compile clean release release-macosx release-windows release-generic clean-release i18n images help javadoc clean-javadoc version
