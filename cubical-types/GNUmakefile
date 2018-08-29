LHS_SOURCES:=$(wildcard *.lhs)
TEX_SOURCES:=$(wildcard *.tex)
TARGETS:=$(LHS_SOURCES:.lhs=.pdf) $(TEX_SOURCES:.tex=.pdf)
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
