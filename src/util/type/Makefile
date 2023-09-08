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

all: $(TYPE_O)

$(TYPE_O): $(TYPE_H) $(TYPE_C)
	$(CC) $(INC) $(CCOPT) -c $(TYPE_C) -o $(TYPE_O)

clean:
	/bin/rm -f *.log *.o
