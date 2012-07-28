# -*- makefile -*-
##########################################
VPATH=../src
EXE=asap-md.x
SRC=main.f90 kinds.f90 constants.f90 io.f90 \
	threading.f90
PPS=messages.F90
OBJ=$(PPS:.F90=.o) $(SRC:.f90=.o)
FCFLAGS=$(OPTFLAGS) $(ARCHFLAGS)
##########################################
##########################################
default: $(EXE)

clean:
	-rm -f $(OBJ) $(EXE) *.mod .depend version.*

.PHONY: clean default version
##########################################
# generate version header
version.f90: $(SRC) $(PPS) ../config/mkversion.sh ../config/Common.mk
	@../config/mkversion.sh FC="$(FC)" FCFLAGS="$(FCFLAGS)"

##########################################
# pattern rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90

%.o: %.F90
	$(FC) $(FPPFLAGS) -c $(FCFLAGS) $<

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

$(EXE): version.o $(OBJ)
	$(FC) -o $@ $(FCFLAGS) $(LINKFLAGS) $^

##########################################
# dependency tracking
.depend: $(PPS) $(SRC)
	@../config/mkdep.pl -o $@ -I ../src $^
-include .depend
