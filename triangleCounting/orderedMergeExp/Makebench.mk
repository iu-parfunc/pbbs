# ********************
# GENERIC MAKEFILE FOR MOST BENCHMARKS THAT LINK
# THE TIMING CODE WITH THE IMPLEMENTATION
# USES FOLLOWING DEFINITIONS
#    BNCHMRK : the name of the benchmark
#    OBJS : implementation object files
#    PCC : the compiler
#    PCFLAGS : compiler flags
#    PLFLAGS : compiler link flags
# ********************
# Path to where source/include files are kept

all: $(BNCHMRK)

SOURCE_DIR ?= ./
include $(SOURCE_DIR)/Makebench.global

vpath %.C $(SOURCE_DIR)
vpath %.C $(SOURCE_DIR)/../common
vpath %.h $(SOURCE_DIR)
vpath %.py $(SOURCE_DIR)

include_dirs := $(SOURCE_DIR):$(SOURCE_DIR)/../common/:$(PLIB)

# pull in dependency info for *existing* .o files

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(OBJS:.o=.d)
endif

# to call - $(call make-depend,source-file,object-file,depend-file)
define make-depend
	@echo "   [DEP] $(notdir $1)"
	@$(PCC) -MM            \
	        -MF $3         \
	        -MP            \
	        -MT $2         \
	        $(PCFLAGS)     \
	        $1
endef


# Make all implementation objects
%.o : %.C 
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	@echo "   [CC] $(notdir $<)"
	@$(PCC) $(PCFLAGS) -c $< -o $@


# Putting together the real benchmark
$(BNCHMRK) : $(TIME).o $(OBJS)
	@echo "   [LNK] $(notdir $@)"
	@$(PCC) $(PLFLAGS) -o $@ $+

clean :
	rm -f $(BNCHMRK) *.o *.pyc

## disabled for now
#cleansrc :
#	make -s clean
#	rm -f $(GLOBAL) $(BENCH) $(TEST_FILES)

