all:
	R CMD Sweave lorentz_JOC.Rnw
	pdflatex lorentz_JOC
	bibtex lorentz_JOC
	pdflatex lorentz_JOC
	pdflatex lorentz_JOC

clean:
	rm -f  *.aux *.log *.out *.bbl *.blg *.Rout *.tex *.pdf

