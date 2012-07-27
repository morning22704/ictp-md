# -*- makefile -*-
##########################################
VPATH=../src
EXE=asap-md.x
SRC=main.f90
OBJ=$(SRC:.f90=.o)
##########################################
FCFLAGS=$(FPPFLAGS) $(OPTFLAGS) $(ARCHFLAGS)
##########################################
default: $(EXE)
clean:
	rm -f $(OBJ) $(EXE) *.mod .depend
##########################################
.SUFFIXES:
.SUFFIXES: .o .f90

.PHONY: default clean
##########################################
.f90.o:
	$(FC) -c $(FCFLAGS) $<

$(EXE): $(OBJ)
	$(FC) -o $@ $(FCFLAGS) $^
##########################################
.depend: $(SRC)
	../config/mkdep.pl -o $@ -I ../src $^
-include .depend
