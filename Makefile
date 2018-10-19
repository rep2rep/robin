MLC=polyc
FLAGS=
ROBIN_TMP:=$(shell mktemp)

all: dist/robin

dist/robin: $(ROBIN_TMP)
	$(MLC) $(FLAGS) -o $@ $<

.PHONY:$(ROBIN_TMP)
$(ROBIN_TMP): base.sml src/main.sml
	for f in $^; do \
		echo "use \"$$f\";" >> $@ ; \
	done

base.sml: src/util/import.sml
	echo 'val BASE="'`pwd`'/src/";' > base.sml
	cat $< >> base.sml
	echo 'import "util.robinlib"; open RobinLib;' >> base.sml

.PHONY:clean
clean:
	rm -rf dist/*
	rm -rf base.sml
