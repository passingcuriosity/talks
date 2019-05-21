# Build PDF documents from LHS and TeX source files.
#
# This file makes some assumptions and adds a few too many dependencies but
# also avoids any need for explicit dependency management.

LHS_SOURCES:=$(shell grep -l "%include polycode.fmt" $(shell find * -name "*.lhs"))
TEX_SOURCES:=$(shell grep -El "[\\]documentclass" $(shell find * -name "*.tex"))
TARGETS:=$(LHS_SOURCES:.lhs=.pdf) $(TEX_SOURCES:.tex=.pdf)

# find(1) operators to match possible dependency files. Add additional patterns
# by following the obvious pattern.
DEP_PATTERN:=-name '*.bib' -o -name '*.dot' -o -name '*.eps' -o -name '*.png'

.SECONDEXPANSION:
%.tex: %.lhs $$(shell find $$(@D) -name '*.lhs' ! -name '$$(<F)')
	cd $(@D) && lhs2TeX -o $(@F) $(<F)

.SECONDEXPANSION:
%.tex: %.md
	cd $(@D) && pandoc -s -o $(@F) $(<F)

.SECONDEXPANSION:
%.pdf: %.tex $$(shell find $$(@D) $(DEP_PATTERN))
	cd $(@D) && \
	lualatex -shell-escape $(<F) && \
	( grep -q "addbibresource" $(<F) && biber $(*F); ) && \
	lualatex -shell-escape $(<F) && \
	lualatex -shell-escape $(<F)

all: $(TARGETS)

clean:
	find . -type f -print | git check-ignore --stdin | xargs rm
