SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR:=import

ifneq ($(DRAKE_RTSROOT),)
HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(BUILDDIR)/test_yaml $(BUILDDIR)/test_serialize

all: $(BUILDDIR)/test_yaml $(BUILDDIR)/test_serialize

$(BUILDDIR)/test_yaml: test_yaml.adb $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR)/test_serialize: test_serialize.adb $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build
	-rm -rf import
	-rm test_yaml.yaml test_serialize.yaml
