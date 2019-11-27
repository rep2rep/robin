SHELL := /bin/bash
MLC=polyc
ifeq (, $(shell which rlwrap))
	REPL=poly
else
	REPL=rlwrap poly
endif
FLAGS=
ROBIN_TMP:=$(shell mktemp)
FINDCORR_TMP:=$(shell mktemp)
ROBIN_VERSION:=$(shell git describe --all --long | rev | cut -d'-' -f 1 | rev)

all: dist/robin dist/findcorr
robin: dist/robin
findcorr: dist/findcorr

dist/robin: $(ROBIN_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

dist/findcorr: $(FINDCORR_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

.PHONY:$(ROBIN_TMP)
$(ROBIN_TMP): base.sml src/main.sml
	echo "use \""$<"\";" >> $@;
	for f in $(filter-out base.sml,$^); do \
		tmp=$$(dirname $$f)/$$(basename $$f .sml); \
		tmp=$$(sed "s/^src\///" <<< $$tmp); \
		tmp=$$(sed "s/\//\./g" <<< $$tmp); \
		echo "import \"$$tmp\";" >> $@ ; \
	done

.PHONY:$(FINDCORR_TMP)
$(FINDCORR_TMP): base.sml src/findcorr.sml
	echo "use \""$<"\";" >> $@;
	for f in $(filter-out base.sml,$^); do \
		tmp=$$(dirname $$f)/$$(basename $$f .sml); \
		tmp=$$(sed "s/^src\///" <<< $$tmp); \
		tmp=$$(sed "s/\//\./g" <<< $$tmp); \
		echo "import \"$$tmp\";" >> $@ ; \
	done

base.sml:
	echo 'val ROBIN_VERSION="'$(ROBIN_VERSION)'";' >> base.sml
	echo 'val BASE="./src/";' >> base.sml
	echo 'use "src/util/robinlib.sml";' >> base.sml

.PHONY:clean
clean:
	rm -rf dist
	rm -rf base.sml

.PHONY:repl
repl: base.sml
	$(REPL) --use $<
