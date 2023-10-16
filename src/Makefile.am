SUFFIXES = .c .mod .def .d .o

DEPFLAGS=-MMD -MT $@ -MP -MF .deps/$*.d

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
DEBUG=-fm2-g -g -Wuninit-variable-checking
M2FLAGS=$(OPT) $(CHECK) $(DEBUG) -fextended-opaque -fgen-module-list=-

BUILT_SOURCES = advflex.c
bin_PROGRAMS = ptower
ptower_SOURCES = AdvCmd.mod \
       AdvIntroduction.mod \
       AdvMap.mod \
       AdvMath.mod \
       AdvSound.mod \
       AdvSystem.mod \
       AdvTreasure.mod \
       AdvUtil.mod \
       AdvParse.mod \
       DrawG.mod \
       DrawL.mod \
       Lock.mod \
       ProcArgs.mod \
       Screen.mod \
       Window.mod \
       SocketControl.c \
       advflex.c

ptower_LDADD = advflex.o SocketControl.o

ptower$(EXEEXT): $(DEPS)
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/Dungeon.mod $(DEPS) -o $@

advflex.c: $(srcdir)/adv.flex
	flex -t $< > $@

# $(srcdir)/AdvParse.mod: $(srcdir)/AdvParse.bnf

.deps: ; @mkdir -p $@

%.o: %.mod .deps/%.d
	@test -z .deps || mkdir -p .deps
	gm2 $(DEPFLAGS) $(M2FLAGS) -I. -I$(srcdir) -c $<

.c.o:
	gcc -c -O0 -g $<

DEPFILES=$(ptower_SOURCES:%.mod=.deps/%.d)

$(DEPFILES):

include $(wildcard $(DEPFILES))
