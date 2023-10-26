#!/usr/bin/make
#
# OpenBDS						October 22, 2023
#
# source: api/fortran/test/sphere/Makefile
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

all: $(TEST_SPHERE_BIN)

$(TEST_SPHERE_BIN): $(TEST_SPHERE_O)
	$(FC) $(FCOPT) $(OBJS) $(TEST_SPHERE_O) -o $(TEST_SPHERE_BIN)

$(TEST_SPHERE_O): $(MODULES) $(OBJS) $(TEST_SPHERE_F)
	$(FC) $(INC) $(IMODS) $(FCOPT) -c $(TEST_SPHERE_F) -o $(TEST_SPHERE_O)

clean:
	/bin/rm -f *.o *.bin
