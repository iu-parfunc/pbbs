include gppDefs

# required files
NEAREST_NEIGHBORS = geometry.h nearestNeighbors.h octTree.h blockRadixSort.h transpose.h
DELAUNAY = topology.h delaunayDefs.h
GLOBAL_REQUIRE = parallel.h sequence.h $(NEAREST_NEIGHBORS) $(DELAUNAY)
LOCAL_REQUIRE = 
OBJS = serialDelaunay.o 

include ../common/timeRequiredFiles
include MakeBench
