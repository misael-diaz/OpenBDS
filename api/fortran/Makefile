#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: api/fortran/Makefile
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

all: OBDS modules tests

modules:
	@$(MAKE) -C module

OBDS: modules
	@$(MAKE) -C bds

tests: modules
	@$(MAKE) -C test

clean:
	@$(MAKE) -C module clean
	@$(MAKE) -C test clean
