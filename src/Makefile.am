SUFFIXES = .c .mod .def .o .obj .lo .a .bnf .flex

MY_DEPS = AdvCmd.o AdvConnect.o \
          AdvIntroduction.o AdvMap.o \
          AdvMath.o AdvParse.o \
          AdvSound.o AdvSystem.o \
          AdvTreasure.o AdvUtil.o \
          AdvParse.o \
          DrawG.o DrawL.o \
          Dungeon.o \
          Lock.o \
          ProcArgs.o \
          Screen.o \
          Window.o \
          SocketControl.o \
          advflex.o

OPT=-O0 -fm2-g -g
CHECK=-fsoft-check-all
M2FLAGS=$(OPT) $(CHECK) -fextended-opaque -flibs=cor,pim,iso

BUILT_SOURCES = advflex.c

advflex.c: $(srcdir)/adv.flex
	flex -t $< > $@

ptower$(EXEEXT): $(MY_DEPS)
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/Dungeon.mod -o $@

# $(srcdir)/AdvParse.mod: $(srcdir)/AdvParse.bnf


.mod.o:
	gm2 $(M2FLAGS) -I. -I$(srcdir) -c $< -o $@
