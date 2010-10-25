SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR=import

ifeq ($(DRAKE),)
H=$(SOURCEDIR)/import.h
RTSDIR=
MFLAGS=
LFLAGS=
else
H=$(IMPORTDIR)/import2.h $(IMPORTDIR)/import-drake.h $(IMPORTDIR)/import-lib.h
RTSDIR=rts
MFLAGS=--RTS=$(abspath $(RTSDIR)) -a
LFLAGS=-largs -lc-gnat
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(BUILDDIR)/test_yaml $(BUILDDIR)/test_serialize

all: $(BUILDDIR)/test_yaml $(BUILDDIR)/test_serialize

$(BUILDDIR)/test_yaml: test_yaml.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR)/test_serialize: test_serialize.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(IMPORTDIR)/c.ads: $(H)
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $<

ifneq ($(DRAKE),)

$(IMPORTDIR)/import2.h:
	-mkdir $(IMPORTDIR)
	echo \#include \"import-drake.h\" > $@
	echo \#include \"import-lib.h\" >> $@

$(IMPORTDIR)/import-drake.h: $(DRAKE)/import.h
	ln -s $(abspath $<) $@

$(IMPORTDIR)/import-lib.h: $(SOURCEDIR)/import.h
	ln -s $(abspath $<) $@

$(RTSDIR):
	make -C $(DRAKE) RTSDIR=$(abspath $(RTSDIR)) IMPORTDIR=$(abspath $(IMPORTDIR))

endif

clean:
	-rm -rf build
	-rm -rf import
	-rm -rf rts
	-rm test_yaml.yaml test_serialize.yaml
