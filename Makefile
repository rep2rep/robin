MLC=polyc
FLAGS=

all: dist/robin

dist/robin: src/main.sml base.sml
	$(MLC) $(FLAGS) -o $@ $<

.PHONY:base.sml
base.sml:
	echo 'val BASE="'`pwd`'/src/";' > base.sml
	cat 'src/util/import.sml' >> base.sml
