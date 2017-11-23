
all: ctt.pdf

ctt.pdf: ctt.tex ctt.bib
	pdflatex ctt
	bibtex ctt
	pdflatex ctt
	pdflatex ctt
