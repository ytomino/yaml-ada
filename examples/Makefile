SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifeq ($(TARGET),$(HOST))
 GCCPREFIX=
 BINLN=bin
else
 GCCPREFIX=$(TARGET)-
 BINLN=
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3cbhms \
	$(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifeq ($(TARGET),$(HOST))
 WITH_YAML?=
 ifneq ($(WITH_YAML),)
  YAML_LFLAGS=-L$(WITH_YAML)/lib
  YAML_HFLAGS=-I$(WITH_YAML)/include
 else
  YAML_LFLAGS:=$(shell pkg-config --libs-only-L yaml-0.1)
  YAML_HFLAGS:=$(shell pkg-config --cflags-only-I yaml-0.1)
 endif
 LFLAGS+=$(YAML_LFLAGS)
 HFLAGS+=$(YAML_HFLAGS)
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
 IMPORTDIR=
 GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
 ifneq ($(wildcard import/$(TARGET)),)
  IMPORTDIR=import/$(TARGET)
 else
  IMPORTDIR=$(BUILDDIR)/import
 endif
 MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),gcc)
 GFLAGS+=$(filter -m32 -m64,$(GCC))
 MFLAGS+=--GCC="$(GCC)"
 LFLAGS+=--GCC="$(GCC)"
 HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check clean $(EXAMPLES)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES)) $(BINLN)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $(basename $(notdir $<)).ali $(GFLAGS) $(BFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(filter-out test_%,$(EXAMPLES)): %: $(BUILDDIR)/% $(BINLN)

check: all $(TESTS)

$(TESTS): %: $(BUILDDIR)/%
	$<

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -r *.noindex bin
