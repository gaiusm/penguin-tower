SUFFIXES = .c .mod .def .d .o

DEPFLAGS=-MMD -MT $@ -MP -MF .deps/$*.d

M2DEPS = AdvCmd.o \
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
       Window.o

DEPS = $(M2DEPS) \
       advflex.o \
       SocketControl.o

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
       SocketControl.c

ptower_LDADD = advflex.o SocketControl.o

ptower$(EXEEXT): $(DEPS)
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/Dungeon.mod $(DEPS) -o $@

# $(srcdir)/AdvParse.mod: $(srcdir)/AdvParse.bnf

.deps:
	mkdir -p $@

%.o: %.mod .deps/%.d
	echo $(DEPFILES)
	test -z .deps || mkdir -p .deps
	gm2 $(DEPFLAGS) $(M2FLAGS) -I. -I$(srcdir) -c $<

.c.o:
	test -z .deps || mkdir -p .deps
	gcc $(DEPFLAGS) -c -O0 -g $<

advflex.c: $(srcdir)/adv.flex
	test -z .deps || mkdir -p .deps
	flex -t $< > $@

DEPFILES=$(ptower_SOURCES:%.mod=.deps/%.d)

$(DEPFILES):
	echo "Value of DEPFILES = $(DEPFILES)"

include $(wildcard $(DEPFILES))
