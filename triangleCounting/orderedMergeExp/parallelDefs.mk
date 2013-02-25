ifdef LONG
INTT = -DLONG
endif

ifdef OPENMP
PCC = g++
PCFLAGS = -fopenmp -O2 -DOPENMP $(INTT)
PLFLAGS = -fopenmp

else ifdef CILK
PCC = cilk++
PCFLAGS = -O2 -DCILK -Wno-cilk-for $(INTT)
PLFLAGS = 

else ifdef ICPC
PCC = icpc
PCFLAGS = -O3 -DCILKP $(INTT)
PLFLAGS = 

else
PCC = g++
PLFLAGS = $(LFLAGS)
PCFLAGS = -O2 $(INTT)
endif

INCLUDE_DIRS = $(patsubst %,-I%,$(subst :, ,$(include_dirs)))
PCFLAGS += -I $(INCLUDE_DIRS)