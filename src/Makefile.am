SUFFIXES = .c .mod .def .o .obj .lo .a .bnf .flex

DEPS = AdvCmd.o \
       AdvIntroduction.o \
       AdvMap.o \
       AdvMath.o \
       AdvSound.o \
       AdvSystem.o \
       AdvTreasure.o \
       AdvUtil.o \
       AdvParse.o \
       DrawG.o \
       DrawL.o \
       Lock.o \
       ProcArgs.o \
       Screen.o \
       Window.o \
       SocketControl.o \
       advflex.o

# OPT=-O0 -fm2-g -g -flto
OPT=-O0
CHECK=-fsoft-check-all
DEBUG=-fm2-g -g
# -flibs=cor,pim,iso
M2FLAGS=$(OPT) $(CHECK) $(DEBUG) -fextended-opaque -fgen-module-list=-

BUILT_SOURCES = advflex.c

advflex.c: $(srcdir)/adv.flex
	flex -t $< > $@

ptower$(EXEEXT): $(DEPS)
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/Dungeon.mod $(DEPS) -o $@

# $(srcdir)/AdvParse.mod: $(srcdir)/AdvParse.bnf


.mod.o:
	gm2 $(M2FLAGS) -I. -I$(srcdir) -c $< -o $@
