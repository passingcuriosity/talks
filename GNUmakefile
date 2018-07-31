SOURCES:=$(wildcard *.lhs)
TARGETS:=$(SOURCES:.lhs=.pdf)
DEPS:=$(wildcard *.bib *.dot *.eps *.png)

%.tex: %.lhs GNUmakefile
	lhs2TeX -o $@ $<

%.pdf: %.tex GNUmakefile $(DEPS)
	pdflatex $<
	biber $(<:.tex=)
	pdflatex $<
	pdflatex $<

all: $(TARGETS)

clean:
	find . -type f -print | xargs git check-ignore | xargs rm
