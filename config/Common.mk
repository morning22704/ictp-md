# -*- makefile -*-
##########################################
VPATH=../src
EXE=asap-md.x
SRC=main.f90 version.f90 kinds.f90 constants.f90 \
	io.f90 threading.f90
PPS=messages.F90
OBJ=$(PPS:.F90=.o) $(SRC:.f90=.o)
##########################################
FCFLAGS=$(OPTFLAGS) $(ARCHFLAGS)
##########################################
default: $(EXE)
clean:
	rm -f $(OBJ) $(EXE) *.mod .depend version.f90
##########################################
.SUFFIXES:
.SUFFIXES: .o .f90 .F90

.PHONY: default clean
##########################################
.F90.o:
	$(FC) $(FPPFLAGS) -c $(FCFLAGS) $<

.f90.o:
	$(FC) -c $(FCFLAGS) $<

$(EXE): $(OBJ)
	$(FC) -o $@ $(FCFLAGS) $(LINKFLAGS) $^

##########################################
version.f90:
	../config/mkversion.sh

.depend: $(PPS) $(SRC)
	../config/mkversion.sh
	../config/mkdep.pl -o $@ -I ../src $^
-include .depend
