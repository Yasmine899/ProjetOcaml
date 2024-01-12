.PHONY: all build format edit demo clean

src?=0
dst?=5
graph?=graph5.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile outfiledot
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile
	dot -Tsvg outfiledot > graph.svg

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
