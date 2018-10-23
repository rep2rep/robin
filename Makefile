MLC=polyc
FLAGS=
ROBIN_TMP:=$(shell mktemp)

all: dist/robin

dist/robin: $(ROBIN_TMP)
	mkdir -p dist
	$(MLC) $(FLAGS) -o $@ $<

$(ROBIN_TMP): base.sml src/main.sml
	for f in $^; do \
		echo "use \"$$f\";" >> $@ ; \
	done

.PHONY:base.sml
base.sml:
	echo 'val BASE="'`pwd`'/src/";' > base.sml
	cat 'src/util/import.sml' >> base.sml

.PHONY:clean
clean:
	rm -rf dist
