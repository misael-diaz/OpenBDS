#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: bds/fortran/Makefile
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

all: $(OBDS)

$(OBDS): $(OBDS_O)
	$(FC) $(INC) $(FCOPT) $(DEPS_O) $(OBDS_O) -o $(OBDS)


$(OBDS_O): $(DEPS_O) $(OBDS_F)
	$(FC) $(INC) $(FCOPT) $(FFREE) -c $(OBDS_F) -o $(OBDS_O)

clean:
	/bin/rm -f *.o *.log *.mod *.bin
