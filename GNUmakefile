LHS_SOURCES:=$(shell grep -l "%include polycode.fmt" $(shell find . -name "*.lhs"))
TEX_SOURCES:=$(shell find . -name "*.tex")
TARGETS:=$(LHS_SOURCES:.lhs=.pdf) $(TEX_SOURCES:.tex=.pdf)
DEPS:=$(wildcard *.bib *.dot *.eps *.png)

%.tex: %.lhs
	cd $(shell dirname $<) && \
	lhs2TeX -o $(shell basename $@) $(shell basename $<)

%.pdf: %.tex $(DEPS)
	cd $(shell dirname $<) && \
	pdflatex $(shell basename $<) && \
	biber $(shell basename $(<:.tex=)) && \
	pdflatex $(shell basename $<) && \
	pdflatex $(shell basename $<)

all: $(TARGETS)

clean:
	find . -type f -print | xargs git check-ignore | xargs rm