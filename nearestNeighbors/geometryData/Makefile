include parallelDefs

COMMON = dataGen.h utils.h IO.h parseCommandLine.h geometry.h geometryIO.h parallel.h sequence.h
GENERATORS = uniform plummer triangles addRays toNodes

.PHONY: all clean
all: $(GENERATORS)

$(COMMON) :
	ln -s ../../common/$@ .

%.o : %.C $(COMMON) 
	$(PCC) $(PCFLAGS) -c $< -o $@

uniform : uniform.C geometryData.h $(COMMON) 
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o $@.exe $@.C

toNodes : toNodes.C $(COMMON) 
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o $@.exe $@.C

plummer : plummer.C geometryData.h $(COMMON) 
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o $@.exe $@.C

triangles : triangles.C geometryData.h $(COMMON) 
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o $@.exe $@.C

addRays : addRays.C $(COMMON) 
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o $@.exe $@.C

clean :
	rm -f *.o $(GENERATORS)
	make clean -s -C data

cleansrc : 
	make -s clean
	rm -f $(COMMON) 
