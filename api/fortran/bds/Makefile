#!/usr/bin/make
#
# OpenBDS						October 22, 2023
#
# source: api/fortran/bds/Makefile
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

all: $(OBDS_BIN)

$(OBDS_BIN): $(OBDS_O)
	$(FC) $(FCOPT) $(OBJS) $(OBDS_O) -o $(OBDS_BIN)

$(OBDS_O): $(MODULES) $(OBJS) $(OBDS_F)
	$(FC) $(INC) $(IMODS) $(FCOPT) -c $(OBDS_F) -o $(OBDS_O)

clean:
	/bin/rm -f *.o *.bin
