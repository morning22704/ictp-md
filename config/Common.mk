# -*- makefile -*-
##########################################
VPATH=../src
EXE=asap-md.x
# sources w/o preprocessing
SRC=main.f90 kinds.f90 constants.f90 io.f90 threading.f90
# sources requiring preprocessing
PPS=messages.F90
# all objects
OBJ=$(PPS:.F90=.o) $(SRC:.f90=.o)
FCFLAGS=$(OPTFLAGS) $(ARCHFLAGS)
##########################################
##########################################
default: version depend compile
compile: $(EXE)

clean:
	-rm -f $(OBJ) $(EXE) *.mod .depend version.*

.PHONY: clean compile default depend version
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
