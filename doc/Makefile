space.pdf: space.tex spacepl.tex georss.tex wgs84.tex freebase.tex dbpedia.tex \
	wkt.tex kml.tex gml.tex spacewebloader.tex
	./runtex --pdf space

space.tex: space.doc
	./doc2tex $< > $@
spacepl.tex: ../prolog/space/space.pl
	swipl pltotex.pl -- --out=spacepl.tex --subsection 'library(space/space)'
georss.tex: ../prolog/space/georss.pl
	swipl pltotex.pl -- --out=georss.tex --subsection 'library(space/georss)'
wgs84.tex: ../prolog/space/wgs84.pl
	swipl pltotex.pl -- --out=wgs84.tex --subsection 'library(space/wgs84)'
freebase.tex: ../prolog/space/freebase.pl
	swipl pltotex.pl -- --out=freebase.tex --subsection 'library(space/freebase)'
dbpedia.tex: ../prolog/space/dbpedia.pl
	swipl pltotex.pl -- --out=dbpedia.tex --subsection 'library(space/dbpedia)'
wkt.tex: ../prolog/space/dbpedia.pl
	swipl pltotex.pl -- --out=wkt.tex --subsection 'library(space/wkt)'
kml.tex: ../prolog/space/kml.pl
	swipl pltotex.pl -- --out=kml.tex --subsection 'library(space/kml)'
gml.tex: ../prolog/space/gml.pl
	swipl pltotex.pl -- --out=gml.tex --subsection 'library(space/gml)'
spacewebloader.tex: ../prolog/space/space_web_loader.pl
	swipl pltotex.pl -- --out=spacewebloader.tex --subsection 'library(space/space_web_loader)'
