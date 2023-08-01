#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: Makefile
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

all: $(TESTS)

$(MAIN_BIN): $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_OBJ)
	$(FC) $(FCOPT) $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_OBJ) -o $(MAIN_BIN)

$(TEST_BIN): $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_OBJ)
	$(CC) $(CCOPT) $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_OBJ) -o $(TEST_BIN) $(LIBS)

$(UTIL_OBJ): $(UTIL_SRC)
	$(CC) $(CCOPT) -c $(UTIL_SRC) -o $(UTIL_OBJ)

$(SPHERE_OBJ): $(UTIL_OBJ) $(SPHERE_SRC)
	$(CC) $(CCOPT) -c $(SPHERE_SRC) -o $(SPHERE_OBJ)

$(TEST_OBJ): $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_SRC)
	$(CC) $(CCOPT) -c $(TEST_SRC) -o $(TEST_OBJ)

$(MAIN_OBJ): $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_SRC)
	$(FC) $(FCOPT) -c $(MAIN_SRC) -o $(MAIN_OBJ)

clean:
	/bin/rm -rf *.o *.mod *.bin
