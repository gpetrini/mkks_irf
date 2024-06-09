# Model compilation options
TARGET=lsd
FUN=fun_model_jeec
FUN_EXTRA=jeec_initialisation.h jeec_aggregate.h jeec_banks.h jeec_firms_capital.h jeec_firms_financial.h jeec_firms_rd.h jeec_government.h jeec_households.h jeec_firms_production.h jeec_demand_distribution.h jeec_labour_market.h jeec_entry_exit.h jeec_checks.h jeec_save.h
SWITCH_CC=-O3 -ggdb3
SWITCH_CC_LNK=

# System compilation options
# LSD options
LSDROOT=/home/gpetrini/LSD
SRC=src

# Libraries options
TCL_VERSION=8.6
PATH_TCLTK_HEADER=/usr/include/tcl$(TCL_VERSION)
PATH_TCLTK_LIB=/usr/lib/tcl$(TCL_VERSION)
TCLTK_LIB=-ltcl$(TCL_VERSION) -ltk$(TCL_VERSION)
PATH_HEADER=.
PATH_LIB=.
LIB=-lz -lpthread

# Compiler options
CC=g++
GLOBAL_CC=-march=native -ffp-contract=off -std=gnu++14 -w
SSWITCH_CC=-fnon-call-exceptions -O3

# Body of makefile (from makefile_linux.txt)
# specify where are the sources of LSD
SRC_DIR=$(LSDROOT)/$(SRC)/

# location of tcl/tk and other headers
INCLUDE=-I$(LSDROOT)/$(SRC) -I$(PATH_TCLTK_HEADER) -I$(PATH_HEADER)

# OS command to delete files
RM=rm -f

# link executable
$(TARGET): $(FUN).o $(SRC_DIR)common.o $(SRC_DIR)lsdmain.o $(SRC_DIR)analysis.o \
$(SRC_DIR)debug.o $(SRC_DIR)draw.o $(SRC_DIR)edit.o $(SRC_DIR)edit_dat.o \
$(SRC_DIR)file.o $(SRC_DIR)interf.o $(SRC_DIR)nets.o $(SRC_DIR)object.o \
$(SRC_DIR)report.o $(SRC_DIR)runtime.o $(SRC_DIR)set_all.o $(SRC_DIR)show_eq.o \
$(SRC_DIR)util.o $(SRC_DIR)variab.o
	$(CC) $(GLOBAL_CC) $(SWITCH_CC) $(FUN).o $(SRC_DIR)common.o $(SRC_DIR)lsdmain.o \
	$(SRC_DIR)analysis.o $(SRC_DIR)debug.o $(SRC_DIR)draw.o $(SRC_DIR)edit.o \
	$(SRC_DIR)edit_dat.o $(SRC_DIR)file.o $(SRC_DIR)interf.o $(SRC_DIR)nets.o \
	$(SRC_DIR)object.o $(SRC_DIR)report.o $(SRC_DIR)runtime.o $(SRC_DIR)set_all.o \
	$(SRC_DIR)show_eq.o $(SRC_DIR)util.o $(SRC_DIR)variab.o \
	$(SWITCH_CC_LNK) -L$(PATH_TCLTK_LIB) $(TCLTK_LIB) -L$(PATH_LIB) $(LIB) -o $(TARGET)

# compile modules
$(FUN).o: $(FUN).cpp $(FUN_EXTRA) model_options.txt $(SRC_DIR)check.h \
$(SRC_DIR)decl.h.gch $(SRC_DIR)fun_head.h $(SRC_DIR)fun_head_fast.h
	$(CC) $(GLOBAL_CC) $(SWITCH_CC) $(INCLUDE) -c $(FUN).cpp -o $(FUN).o
	
$(SRC_DIR)common.o: $(SRC_DIR)common.cpp $(SRC_DIR)common.h $(SRC_DIR)system_options.txt
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)common.cpp -o $(SRC_DIR)common.o

$(SRC_DIR)lsdmain.o: $(SRC_DIR)lsdmain.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)lsdmain.cpp -o $(SRC_DIR)lsdmain.o

$(SRC_DIR)analysis.o: $(SRC_DIR)analysis.cpp $(SRC_DIR)tables.h $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)analysis.cpp -o $(SRC_DIR)analysis.o

$(SRC_DIR)debug.o: $(SRC_DIR)debug.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)debug.cpp -o $(SRC_DIR)debug.o

$(SRC_DIR)draw.o: $(SRC_DIR)draw.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)draw.cpp -o $(SRC_DIR)draw.o

$(SRC_DIR)edit.o: $(SRC_DIR)edit.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)edit.cpp -o $(SRC_DIR)edit.o

$(SRC_DIR)edit_dat.o: $(SRC_DIR)edit_dat.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)edit_dat.cpp -o $(SRC_DIR)edit_dat.o

$(SRC_DIR)file.o: $(SRC_DIR)file.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)file.cpp -o $(SRC_DIR)file.o

$(SRC_DIR)interf.o: $(SRC_DIR)interf.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)interf.cpp -o $(SRC_DIR)interf.o

$(SRC_DIR)nets.o: $(SRC_DIR)nets.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)nets.cpp -o $(SRC_DIR)nets.o

$(SRC_DIR)object.o: $(SRC_DIR)object.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)object.cpp -o $(SRC_DIR)object.o

$(SRC_DIR)report.o: $(SRC_DIR)report.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)report.cpp -o $(SRC_DIR)report.o

$(SRC_DIR)runtime.o: $(SRC_DIR)runtime.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)runtime.cpp -o $(SRC_DIR)runtime.o

$(SRC_DIR)set_all.o: $(SRC_DIR)set_all.cpp $(SRC_DIR)tables.h $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)set_all.cpp -o $(SRC_DIR)set_all.o

$(SRC_DIR)show_eq.o: $(SRC_DIR)show_eq.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)show_eq.cpp -o $(SRC_DIR)show_eq.o

$(SRC_DIR)util.o: $(SRC_DIR)util.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)util.cpp -o $(SRC_DIR)util.o

$(SRC_DIR)variab.o: $(SRC_DIR)variab.cpp $(SRC_DIR)decl.h.gch
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)variab.cpp -o $(SRC_DIR)variab.o

$(SRC_DIR)decl.h.gch: $(SRC_DIR)decl.h $(SRC_DIR)common.h $(SRC_DIR)system_options.txt
	$(CC) $(GLOBAL_CC) $(SSWITCH_CC) $(INCLUDE) -c $(SRC_DIR)decl.h -o $(SRC_DIR)decl.h.gch

# remove compiled files
clean:
	$(RM) $(SRC_DIR)*.o $(SRC_DIR)*.gch $(FUN).o $(TARGET)
