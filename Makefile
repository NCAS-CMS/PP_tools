default:
	@echo "To make pp_rdiff type one of the following:"
	@echo "   make Linux		for Linux  systems"
	@echo "   make SunOS		for SunOS systems"
	@echo "   make HECToR		for HECToR"
	@echo "   make Jasmin		for Jasmin"

	@echo "If your system is not on this list you must edit the makefile"

Linux:
	make targets \
	"F90C = gfortran" \
	"FFLAGS = -c -std=f2003 -fconvert=big-endian"

SunOS:
	make targets \
	"F90C = f95" \
	"FFLAGS = -c -C -g -xfilebyteorder=big4:%all"

HECToR:
	make targets \
	"F90C = ftn" \
	"FFLAGS = -c -O3"

Jasmin:
	make targets \
	"F90C = gfortran" \
	"FFLAGS = -c -O3"	
	



clean:
	-rm -f *.o *.lst *.mod

#######################################

LIBS = 

targets: pp_merge pp_rdiff  pp_qdiff pp_getfields

pp_rdiff: pp_rdiff.o
	${F90C} -Bstatic -o pp_rdiff pp_rdiff.o PP_type.o

pp_rdiff.o: pp_rdiff.f90 pp_type.mod
	$(F90C) $(FFLAGS) pp_rdiff.f90

pp_type.mod: PP_type.f90
	$(F90C) $(FFLAGS) PP_type.f90


##
pp_merge: pp_merge.o
	${F90C} -Bstatic -o pp_merge pp_merge.o

pp_merge.o: pp_merge.f90
	$(F90C) $(FFLAGS) pp_merge.f90


##
pp_qdiff: pp_qdiff.o pp_type.mod
	${F90C} -Bstatic -o pp_qdiff pp_qdiff.o PP_type.o

pp_qdiff.o: pp_qdiff.f90 pp_type.mod
	$(F90C) $(FFLAGS) pp_qdiff.f90

##
pp_getfields: pp_getfields.o
	${F90C} -Bstatic -o pp_getfields pp_getfields.o stash_list.o  PP_type.o

pp_getfields.o: pp_getfields.f90 stash_list.mod pp_type.mod
	$(F90C) $(FFLAGS) pp_getfields.f90

stash_list.mod: stash_list.f90
	$(F90C) $(FFLAGS) stash_list.f90
