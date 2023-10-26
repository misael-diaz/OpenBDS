#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: test/particle-sphere/Makefile
# author: @misael-diaz
#
# Synopsis:
# Defines the Makefile for building the program with GNU make.
#
# Copyright (c) 2023 Misael Diaz-Maldonado
# This file is released under the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#

include make-inc

all: $(TEST) $(FEST)

$(TEST): $(TEST_O)
	$(CC) $(INC) $(CCOPT) $(DEPS_O) $(TEST_O) -o $(TEST) $(LIBS)

$(FEST): $(FEST_O)
	$(FC) $(INC) $(FCOPT) $(DEPS_O) $(FEST_O) -o $(FEST)

$(TEST_O): $(DEPS_O) $(TEST_C)
	$(CC) $(INC) $(CCOPT) -c $(TEST_C) -o $(TEST_O)

$(FEST_O): $(DEPS_O) $(TEST_F)
	$(FC) $(INC) $(FCOPT) $(FFREE) -c $(TEST_F) -o $(FEST_O)

clean:
	/bin/rm -f *.o *.log *.mod *.bin
