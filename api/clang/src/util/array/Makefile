#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: util/array/Makefile
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

all: $(ARRAY_O)

$(ARRAY_O): $(HEADERS) $(ARRAY_C)
	$(CC) $(INC) $(CCOPT) -c $(ARRAY_C) -o $(ARRAY_O)

clean:
	/bin/rm -f *.log *.o
