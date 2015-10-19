all:
	pdflatex lrec2016_listenmaa_claessen.tex
	bibtex lrec2016_listenmaa_claessen
	pdflatex lrec2016_listenmaa_claessen.tex
	pdflatex lrec2016_listenmaa_claessen.tex

xe:
	xelatex lrec2016_listenmaa_claessen.tex

word_count:
	cat *.tex | egrep "^([A-Za-z]|.(item|paragraph|cite))" | wc -w

# Show word-level diff.
# To switch on/off line wrapping use -S in the pager.
diff:
	git diff --word-diff

show:
	git show --word-diff

clean:
	rm *.aux *.log *.dvi *.fdb_latexmk *.bbl *.toc *.fls
