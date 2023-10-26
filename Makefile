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

# sets the module flag according to the FORTRAN Compiler FC
ifeq ($(FC), ifort)
	FMOD = -module
else
	FMOD = -J
endif

export CC
export FC
export CCOPT
export FCOPT
export FMOD
export LIBS

all: apis

apis:
	@$(MAKE) -C api

clean:
	@$(MAKE) -C api clean
