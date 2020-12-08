SUFFIXES = .pnm .n .png

IMAGES = wallv.png wallh.png doorv.png doorh.png \
         hingeh.png hingev.png \
         mann.png mane.png mans.png manw.png \
         manN.png manE.png manS.png manW.png \
         treasure.png arrown.png arrows.png \
         arrowe.png arroww.png

ALLIMAGES: $(IMAGES)

.n.png:
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

hingeh.png: hingeh.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

hingev.png: hingev.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm | pnmrotate 90 > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

mann.png: mane.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm | pnmrotate 90 > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

mans.png: manw.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm | pnmrotate 90 > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

manN.png: manE.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm | pnmrotate 90 > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm

manS.png: manW.n
	groff -I$(srcdir) -M$(srcdir) < $< > t.ps
	echo "" | gs -q -dBATCH -dSAFER -sDEVICE=pnmraw -r100 -sOutputFile=t.pnm t.ps
	pnmcrop t.pnm | pnmrotate 90 > t1.pnm
	pnmscale -xsize=20 -ysize=20 t1.pnm > t2.pnm
	pnmtopng t2.pnm > $@
	$(RM) t.pnm t1.pnm t2.pnm
