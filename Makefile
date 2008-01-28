SRCDIR:=src
LIBDIR:=lib
CLASSDIR:=classes
TMPDIR:=tmp
JDOCDIR:=javadoc
DOCDIR:=doc
DISTDIR:=dist

VERSION:=3.1.0-svn$(shell svnversion | perl -pe 's/(\d+:)?(\d+[MS]?)/$$2/; s/(\d+)M/$$1+1/e')

#CLASSPATH:=$(CLASSDIR):$(LIBDIR)/*
CLASSPATH:=$(CLASSDIR):$(shell echo $(LIBDIR)/*.jar | tr ' ' ':')

#JAVAPATH:=/usr/lib/jvm/java-1.6.0-sun
JAVAPATH:=/usr/lib/jvm/java-1.5.0-sun

JC:=$(JAVAPATH)/bin/javac
JCFLAGS:=-d $(CLASSDIR) -source 5 -Xlint -classpath $(CLASSPATH) \
				 -sourcepath $(SRCDIR)

JAR:=$(JAVAPATH)/bin/jar
JDOC:=$(JAVAPATH)/bin/javadoc

SOURCES:=$(shell find $(SRCDIR) -name '*.java' | sed "s/^$(SRCDIR)\///")
CLASSES:=$(SOURCES:.java=.class)
JARS:=Vengine.jar docs.jar

vpath %.class $(shell find $(CLASSDIR) -type d)
vpath %.java  $(shell find $(SRCDIR) -type d -name .svn -prune -o -print)
vpath %.jar $(LIBDIR)

all: $(CLASSDIR) $(CLASSES) i18n

$(CLASSDIR):
	mkdir -p $(CLASSDIR)

%.class: %.java
	$(JC) $(JCFLAGS) $<

i18n: $(CLASSDIR)
	for i in `cd $(SRCDIR) && find VASSAL -name '*.properties'`; do cp $(SRCDIR)/$$i $(CLASSDIR)/$$i; done

#fast:
#	$(JC) $(JCFLAGS) $(shell find $(SRCDIR) -name '*.java')

#show:
#	echo $(patsubst %,-C $(TMPDIR)/doc %,$(wildcard $(TMPDIR)/doc/*)) 

Vengine.jar: all
	$(JAR) cvfm $(LIBDIR)/$@ dist/Vengine.mf images/* -C $(CLASSDIR) .

docs.jar:
	mkdir -p $(TMPDIR)
	svn export $(DOCDIR) $(TMPDIR)/doc
	find $(TMPDIR)/doc -type f | sed "s/^$(TMPDIR)\/doc\///" >$(TMPDIR)/doc/docsList
	cp dist/docsInfo $(TMPDIR)/doc
	$(JAR) cvf $(LIBDIR)/$@ -C $(TMPDIR)/doc .

version:
	sed -ri 's/VERSION = ".*"/VERSION = "$(VERSION)"/' $(SRCDIR)/VASSAL/Info.java

#installer:
#	$(JAR) cevf VASSAL/launch/install/InstallWizard InstallVASSAL.jar -C $(CLASSDIR) VASSAL/launch/ VASSAL/chat/HttpRequestWrapper*

release: version all $(JARS)
	mkdir -p $(TMPDIR)/VASSAL-$(VERSION) $(TMPDIR)/VASSAL-$(VERSION)/ext
	svn export $(LIBDIR) $(TMPDIR)/VASSAL-$(VERSION)/lib
	cp $(LIBDIR)/Vengine.jar $(LIBDIR)/docs.jar $(TMPDIR)/VASSAL-$(VERSION)/lib
	cp dist/VASSAL.sh dist/VASSAL.bat dist/VASSAL.exe $(TMPDIR)/VASSAL-$(VERSION)
	cd $(TMPDIR) ; zip -9rv VASSAL-$(VERSION).zip VASSAL-$(VERSION) ; cd ..

clean-release:
	$(RM) -r $(TMPDIR)/* $(LIBDIR)/Vengine.jar $(LIBDIR)/docs.jar

javadoc:
	$(JDOC) -d $(JDOCDIR) -link http://java.sun.com/javase/6/docs/api -sourcepath $(SRCDIR) -subpackages $(shell echo $(notdir $(wildcard src/*)) | tr ' ' ':')

clean-javadoc:
	$(RM) -r $(JDOCDIR)

clean: clean-release
	$(RM) -r $(CLASSDIR)/*

.PHONY: all clean release clean-release i18n javadoc clean-javadoc version
