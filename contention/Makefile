include parallelDefs

#COMMON = parallel.h sequence.h sequenceIO.h parseCommandLine.h utils
COMMON = IO.h sequenceIO.h parseCommandLine.h parallel.h quickSort.h deterministicHash.h runTests.py sequence.h utils.h gettime.h

contention: contention.C $(COMMON)
	$(PCC) $(PCFLAGS) $(PLFLAGS) -o contention contention.C

$(COMMON):
	ln -s ../common/$@

clean: 
	rm contention

cleansrc:
	make -s clean
	rm -f $(COMMON) *.pyc