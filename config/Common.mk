# -*- makefile -*-
##########################################
VPATH=../src
NAME=ictp-md
VERSION=08-2012
EXE=$(NAME).x
DOC=../$(NAME)-manual.pdf
# sources w/o preprocessing
SRC=main.f90 io.f90 kinds.f90 constants.f90 memory.f90 threading.f90 \
	input.f90 control.f90 sysinfo.f90
# sources requiring preprocessing
PPS=message_passing.F90
# all objects
OBJ=$(PPS:.F90=.o) $(SRC:.f90=.o)
FCFLAGS=$(OPTFLAGS) $(ARCHFLAGS)
##########################################
##########################################
default: version depend compile
all: compile check doc
compile: $(EXE)

clean:
	-rm -rf latex Doxyfile
	-rm -f $(OBJ) $(EXE) $(DOC) version.o
	-rm -f *.mod .depend .ver1 .ver2 version.f90 *.log

.PHONY: clean compile default depend version doc
##########################################
# generate version header
version.f90: .ver1 $(SRC) $(PPS) ../config/mkversion.sh ../config/Common.mk
	@sh ../config/mkversion.sh $(NAME) $(VERSION) \
		FC="$(FC)" FCFLAGS="$(FCFLAGS)"

.ver1:
	@git log -n 1 --pretty=oneline > .ver1

version:
	@git log -n 1 --pretty=oneline > .ver2
	@cmp -s .ver1 .ver2 || cp .ver2 .ver1

##########################################
# run unit tests
check: $(EXE)
	perl ../config/runtests.pl $(CMD) ./$(EXE) ../tests 

##########################################
# generate documentation
doc: $(DOC)

Doxyfile: ../config/Doxyfile.in ../config/Common.mk
	sed -e s,@VERSION@,$(VERSION),g -e s,@NAME@,$(NAME),g $< > $@

$(DOC): Doxyfile $(SRC) $(PPS)
	doxygen Doxyfile || rm -f $@
	make -C latex || rm -f $@
	cp -p latex/refman.pdf $@ || rm -f $@
	@echo '#####################################'
	@echo 'Doxygen errors and warnings:'
	@cat doxygen.log

##########################################
# pattern rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90

.F90.o:
	$(FC) $(FPPFLAGS) -c $(FCFLAGS) $<

.f90.o:
	$(FC)  -c $(FCFLAGS) $<

$(EXE): version.o $(OBJ)
	$(FC)  -o $@ $(FCFLAGS) $(LINKFLAGS) $^

##########################################
# dependency tracking
depend: .depend version
.depend: version.f90 $(PPS) $(SRC)
	@perl -w ../config/mkdep.pl -I ../src $^
-include .depend
