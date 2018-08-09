filename=hyperspace

pdf: links clean
	xelatex ${filename}
	xelatex ${filename}

quick:
	pdflatex ${filename}

clean:
	rm -f ${filename}.log ${filename}.aux ${filename}.out ${filename}.bbl
	rm -f ${filename}.blg ${filename}.fls ${filename}.ilg ${filename}.nlo
	rm -f ${filename}.nav rm -f ${filename}.snm rm -f ${filename}.toc
	rm -f ${filename}.nls ${filename}.fdb_latexmk
	rm -f .${filename}.tex.swp

links:
	ln -sf ~/Git/zachs_macros/zachs_macros.tex zachs_macros.tex
	ln -sf ~/Git/zachs_macros/numdef.sty numdef.sty
