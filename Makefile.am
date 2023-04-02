SUBDIRS = src data
prefix= @prefix@
datarootdir= @datarootdir@
srcdir= @srcdir@
mandir= @mandir@
exec_prefix=@exec_prefix@
bindir= @bindir@
INSTALL_PROGRAM = install
dist_doc_DATA = README

all-local: force
	cd data ; make ALLIMAGES
	cd src ; make ptower$(EXEEXT)

install-exec-local:
	$(INSTALL_PROGRAM) -m 755 src/ptower $(DESTDIR)/$(bindir)

force:
