ALL := $(wildcard *.md)


%.pdf: %.md GNUmakefile
	pandoc -s -f markdown+smart -t beamer -o $@ $<

%.tex: %.lhs GNUmakefile
	lhs2TeX -o $@ $<

%.pdf: %.tex GNUmakefile
	pdflatex $<
	pdflatex $<

all: presentation2.pdf recursion.pdf

recursion.pdf: recursion.tex $(wildcard *.hs)

clean:
	rm -f *.pdf