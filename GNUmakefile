%.pdf: %.md GNUmakefile
	pandoc -s -f markdown+smart -t beamer -o $@ $<

%.tex: %.lhs GNUmakefile
	lhs2TeX -o $@ $<

%.pdf: %.tex GNUmakefile
	pdflatex $<
	biber $(<:.tex=)
	pdflatex $<
	pdflatex $<

SOURCES:=$(wildcard *.lhs)
TARGETS:=$(SOURCES:.lhs=.pdf)

all: $(TARGETS)

clean:
	find . -type f -print | xargs git check-ignore | xargs rm