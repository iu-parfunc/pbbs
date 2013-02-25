-include default.config

## Load benchmark-specific setup
include ../common/recipe.mk

## Load default benchmark Makefile
include Makebench.global

TARGETS := gpp cilk icpc openmp

.PHONY: $(TARGETS)

MYPATH = $(abspath .)

define make-target
	@echo "Making $(BNCHMRK) using $1"
	mkdir -p .build/$2
	cd .build/$2; make -f ../../Makefile.target PLIB=$(MYPATH)/$(PLIB) $3
	test -e ./$(BNCHMRK)-$2 || ln -s .build/$2/$(BNCHMRK) ./$(BNCHMRK)-$2
endef

all: $(TARGETS)

gpp:
	$(call make-target,g++,gpp,)
cilk:
	$(call make-target,cilk++,cilk++,CILK=1)
icpc:
	$(call make-target,icpc,icpc,ICPC=1)
openmp:
	$(call make-target,g++-omp,omp,OPENMP=1)

link: $(TIME).C $(GLOBAL_REQUIRE) $(BENCH_REQUIRE) $(LOCAL_REQUIRE) $(TIME_GLOBAL_REQUIRE) $(TIME_REQUIRE)


