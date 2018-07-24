ALL := $(wildcard *.md)


%.pdf: %.md GNUmakefile
	pandoc -s -f markdown+smart -t beamer -o $@ $<

%.pdf: %.tex GNUmakefile
	pdflatex $<
	pdflatex $<

all: presentation.pdf recursion.pdf

recursion.pdf: recursion.tex $(wildcard *.hs)

clean:
	rm -f *.pdf