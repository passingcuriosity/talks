SOURCES:=$(wildcard *.lhs)
TARGETS:=$(SOURCES:.lhs=.pdf)
DEPS:=$(wildcard *.bib)

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
