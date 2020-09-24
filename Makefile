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
UNIONTABLE_TMP:=$(shell mktemp)
PSEUDOTABLE_TMP:=$(shell mktemp)
TEST_TMP:=$(shell mktemp)
ROBIN_VERSION:=$(shell git describe --all --long | rev | cut -d'-' -f 1 | rev)

all: robin findcorr uniontables pseudotable
robin: dist/robin
findcorr: dist/findcorr
uniontables: dist/uniontables
pseudotable: dist/pseudotable

dist/robin: $(ROBIN_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

dist/findcorr: $(FINDCORR_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

dist/uniontables: $(UNIONTABLE_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

dist/pseudotable: $(PSEUDOTABLE_TMP)
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

.PHONY:$(UNIONTABLE_TMP)
$(UNIONTABLE_TMP): base.sml src/uniontables.sml
	echo "use\""$<"\";" >> $@;
	for f in $(filter-out base.sml,$^); do \
		tmp=$$(dirname $$f)/$$(basename $$f .sml); \
		tmp=$$(sed "s/^src\///" <<< $$tmp); \
		tmp=$$(sed "s/\//\./g" <<< $$tmp); \
		echo "import \"$$tmp\";" >> $@ ; \
	done

.PHONY:$(PSEUDOTABLE_TMP)
$(PSEUDOTABLE_TMP): base.sml src/pseudotable.sml
	echo "use\""$<"\";" >> $@;
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


test: tests/test
	$<

tests/test: $(TEST_TMP)
	$(MLC) $(FLAGS) -o $@ $<

.PHONY:$(TEST_TMP)
$(TEST_TMP): base.sml tests/tests.sml
	echo "use\""$<"\";" >> $@;
	for f in $(filter-out base.sml,$^); do \
		tmp=$$(dirname $$f)/$$(basename $$f); \
		tmp=$$(sed "s/^src\///" <<< $$tmp); \
		echo "use \"$$tmp\";" >> $@ ; \
	done


.PHONY:clean
clean:
	rm -rf dist
	rm -rf base.sml

.PHONY:repl
repl: base.sml
	$(REPL) --use $<
