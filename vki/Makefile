filename=hyperspace

pdf:
	xelatex ${filename}
	xelatex ${filename}

quick:
	pdflatex ${filename}

clean:
	rm -f ${filename}.log ${filename}.aux ${filename}.out ${filename}.bbl
	rm -f ${filename}.blg ${filename}.fls ${filename}.ilg ${filename}.nlo
	rm -f ${filename}.nav rm -f ${filename}.snm rm -f ${filename}.toc
	rm -f ${filename}.nls ${filename}.vrb ${filename}.fdb_latexmk
	rm -f .${filename}.tex.swp

links:
	cp -f ~/Git/zachs_macros/zachs_macros.tex zachs_macros.tex
	cp -f ~/Git/zachs_macros/numdef.sty numdef.sty
	cp -f ~/Git/zdr.sty/beamerthemezdr.sty .
	cp -f ~/Git/zdr.sty/beamercolorthemezdr.sty .
	cp -f ~/Git/zdr.sty/beamerinnerthemezdr.sty .
	cp -f ~/Git/zdr.sty/beamerouterthemezdr.sty .
	cp -f ~/Git/zdr.sty/SU_Seal_Black.eps .
