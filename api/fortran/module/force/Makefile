#!/usr/bin/make
#
# OpenBDS						October 21, 2023
#
# source: api/fortran/module/force/Makefile
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

all: $(FORCE_O)

$(FORCE_O): $(MODULES) $(FORCE_F)
	$(FC) $(FCOPT) -ffixed-form -c $(FORCE_F) -o $(FORCE_O) -J $(MODS)

clean:
	/bin/rm -f *.o
