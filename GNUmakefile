ALL := $(wildcard *.md)


%.pdf: %.md GNUmakefile
	pandoc -s -f markdown+smart -t beamer -o $@ $<

%.tex: %.lhs GNUmakefile
	lhs2TeX -o $@ $<

%.pdf: %.tex GNUmakefile
	pdflatex $<
	biber $(<:.tex=)
	pdflatex $<
	pdflatex $<

all: presentation2.pdf recursion.pdf

presentation2.pdf: presentation2.tex presentation.bib GNUmakefile

recursion.pdf: recursion.tex $(wildcard *.hs)

clean:
	rm -f *.pdf