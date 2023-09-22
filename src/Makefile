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

export CC
export FC
export CCOPT
export FCOPT
export LIBS

all: utils system-utils particle-utils tests OBDS

$(MAIN_BIN): $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_OBJ)
	$(FC) $(FCOPT) $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_OBJ) -o $(MAIN_BIN)

$(TEST_BIN): $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_OBJ)
	$(CC) $(CCOPT) $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_OBJ) -o $(TEST_BIN) $(LIBS)

$(UTIL_OBJ): $(SYSTEM_H) $(UTIL_H) $(UTIL_SRC)
	$(CC) $(CCOPT) -c $(UTIL_SRC) -o $(UTIL_OBJ)

$(SPHERE_OBJ): $(SPHERE_H) $(UTIL_OBJ) $(SPHERE_SRC)
	$(CC) $(CCOPT) -c $(SPHERE_SRC) -o $(SPHERE_OBJ)

$(TEST_OBJ): $(UTIL_OBJ) $(SPHERE_OBJ) $(TEST_SRC)
	$(CC) $(CCOPT) -c $(TEST_SRC) -o $(TEST_OBJ)

$(MAIN_OBJ): $(FMACROS_H) $(UTIL_OBJ) $(SPHERE_OBJ) $(MAIN_SRC)
	$(FC) $(FCOPT) -c $(MAIN_SRC) -o $(MAIN_OBJ)

utils:
	@$(MAKE) -C util

system-utils:
	@$(MAKE) -C system

particle-utils: utils
	@$(MAKE) -C particles

tests: utils
	@$(MAKE) -C test

OBDS: utils
	@$(MAKE) -C bds

clean:
	/bin/rm -rf *.o *.log *.mod *.bin
	@$(MAKE) -C util clean
	@$(MAKE) -C system clean
	@$(MAKE) -C particles clean
	@$(MAKE) -C test clean
	@$(MAKE) -C bds clean
