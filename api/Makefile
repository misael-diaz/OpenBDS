#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: api/Makefile
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

all: api-clang api-fortran

api-clang:
	@$(MAKE) -C clang

api-fortran:
	@$(MAKE) -C fortran

clean:
	@$(MAKE) -C clang clean
	@$(MAKE) -C fortran clean
