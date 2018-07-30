%.pdf: %.md GNUmakefile
	pandoc -s -f markdown+smart -t beamer -o $@ $<

%.tex: %.lhs GNUmakefile
	lhs2TeX -o $@ $<

%.pdf: %.tex GNUmakefile
	pdflatex $<
	biber $(<:.tex=)
	pdflatex $<
	pdflatex $<

all: presentation.pdf

presentation.pdf: presentation.tex presentation.bib GNUmakefile

clean:
	find . \( -name .git -prune \) -o \( -type f -print \) | xargs git check-ignore | xargs rm