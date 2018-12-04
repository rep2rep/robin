MLC=polyc
ifeq (, $(shell which rlwrap))
	REPL=poly
else
	REPL=rlwrap poly
endif
FLAGS=
ROBIN_TMP:=$(shell mktemp)

all: dist/robin

dist/robin: $(ROBIN_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

.PHONY:$(ROBIN_TMP)
$(ROBIN_TMP): base.sml src/main.sml
	for f in $^; do \
		echo "use \"$$f\";" >> $@ ; \
	done

base.sml:
	echo 'val BASE="'`pwd`'/src/";' > base.sml
	echo 'use "src/util/robinlib.sml"; open RobinLib;' >> base.sml

.PHONY:clean
clean:
	rm -rf dist
	rm -rf base.sml

.PHONY:repl
repl: base.sml
	$(REPL) --use base.sml
