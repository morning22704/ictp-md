# -*- makefile -*-
##########################################
VPATH=../src
EXE=ictp-md.x
DOC=../ictp-md-manual.pdf
# sources w/o preprocessing
SRC=main.f90 io.f90 kinds.f90 constants.f90 memory.f90 threading.f90 \
	read_input.f90 sysinfo.f90
# sources requiring preprocessing
PPS=message_passing.F90
# all objects
OBJ=$(PPS:.F90=.o) $(SRC:.f90=.o)
FCFLAGS=$(OPTFLAGS) $(ARCHFLAGS)
##########################################
##########################################
default: depend version compile
compile: $(EXE)

clean:
	-rm -rf latex
	-rm -f $(OBJ) $(EXE) $(DOC) *.mod .depend version.* *.log

.PHONY: clean compile default depend version doc
##########################################
# generate version header
version.f90: $(SRC) $(PPS) ../config/mkversion.sh ../config/Common.mk
	@../config/mkversion.sh FC="$(FC)" FCFLAGS="$(FCFLAGS)"

version:
	@-git log -n 1 --pretty="     write (stdout,*) 'Last commit  : %H'" > .ver1
	@-grep commit version.f90 > .ver2 2> /dev/null
	@cmp -s .ver1 .ver2 || rm -f version.f90
	@-rm  .ver1 .ver2

##########################################
# generate version header
doc: $(DOC) 
$(DOC): ../config/Doxyfile $(SRC) $(PPS)
	doxygen ../config/Doxyfile || rm -f $@
	make -C latex || rm -f $@
	cp -p latex/refman.pdf $@ || rm -f $@
	@echo '#####################################'
	@echo 'Doxygen errors and warnings:'
	@cat doxygen.log

##########################################
# pattern rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90

%.o: %.F90
	$(FC) $(FPPFLAGS) -c $(FCFLAGS) $<

%.o: %.f90
	$(FC)  -c $(FCFLAGS) $<

$(EXE): version.o $(OBJ)
	$(FC)  -o $@ $(FCFLAGS) $(LINKFLAGS) $^

##########################################
# dependency tracking
.depend depend: $(PPS) $(SRC) version.f90
	@perl -w ../config/mkdep.pl -I ../src $^
-include .depend
