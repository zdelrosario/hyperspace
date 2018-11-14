SHELL := /bin/bash
filename=notes

pdf: 
	xelatex ${filename}
	bibtex ${filename}||true
	xelatex ${filename}
	xelatex ${filename}

quick:
	pdflatex ${filename}

clean:
	rm -rf ${filename}.{log,aux,out,bbl,blg,fls,ilg,nlo,nls,nlg}
	rm -rf ${filename}.{fdb_latexmk,stderr.log,stdout.log,tex.swp}
	rm -rf ${fileaux}.{log,aux,out,bbl,blg,fls,ilg,nlo,nls,nlg}
	rm -rf ${fileaux}.{fdb_latexmk,stderr.log,stdout.log,tex.swp}

links:
	# Macros
	cp ~/Git/zachs_macros/zachs_macros.tex zachs_macros.tex
	cp ~/Git/zachs_macros/numdef.sty numdef.sty
	# Bibtex database
	cp ~/Git/pi_space/pubs.bib bibtex_database.bib
