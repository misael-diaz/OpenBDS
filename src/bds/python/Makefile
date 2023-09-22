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

all: $(LOBDS_SO)

$(LOBDS_SO): $(OBJECTS)
	$(CC) $(INC) $(CCOPT) -shared $(OBJECTS) -o $(LOBDS_SO)

clean:
	/bin/rm -f *.o *.so *.log *.mod *.bin
