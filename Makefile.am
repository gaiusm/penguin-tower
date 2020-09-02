SUBDIRS = src data
dist_doc_DATA = README

all-local: force
	cd src ; make ptower$(EXEEXT)
	cd data ; make ALLIMAGES

install-exec-local:
	echo hello

force:
