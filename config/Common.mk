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
	rm -f $(OBJ) $(EXE) *.mod .depend version.*

.PHONY: default clean version

##########################################
# generate version header
version.f90: $(OBJ)
	../config/mkversion.sh FC="$(FC)" FCFLAGS="$(FCFLAGS)"

##########################################
# pattern rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90

.F90.o:
	$(FC) $(FPPFLAGS) -c $(FCFLAGS) $<

.f90.o:
	$(FC) -c $(FCFLAGS) $<

$(EXE): version.o $(OBJ)
	$(FC) -o $@ $(FCFLAGS) $(LINKFLAGS) $^

##########################################
# dependency tracking
.depend: $(PPS) $(SRC) version.f90
	../config/mkdep.pl -o $@ -I ../src $^
-include .depend
