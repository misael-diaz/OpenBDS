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

export CC
export FC
export CCOPT
export FCOPT
export LIBS

all: tests OBDS

utils:
	@$(MAKE) -C util

system-utils:
	@$(MAKE) -C system

particle-utils: utils system-utils
	@$(MAKE) -C particles

tests: particle-utils
	@$(MAKE) -C test

OBDS: particle-utils
	@$(MAKE) -C bds

clean:
	@$(MAKE) -C util clean
	@$(MAKE) -C system clean
	@$(MAKE) -C particles clean
	@$(MAKE) -C test clean
	@$(MAKE) -C bds clean
