#!/usr/bin/make
#
# OpenBDS						October 29, 2023
#
# source: api/fortran/module/io/Makefile
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

all: $(IO_O)

$(IO_O): $(MODULES) $(IO_F)
	$(FC) $(FCOPT) -c $(IO_F) -o $(IO_O) $(FMOD) $(MODS)

clean:
	/bin/rm -f *.o
