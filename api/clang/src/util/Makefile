#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: util/Makefile
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

all: arrays vectors PRNGs particles

arrays:
	@$(MAKE) -C array
vectors:
	@$(MAKE) -C vector
PRNGs:
	@$(MAKE) -C random
particles:
	@$(MAKE) -C particle

clean:
	@$(MAKE) -C array clean
	@$(MAKE) -C vector clean
	@$(MAKE) -C random clean
	@$(MAKE) -C particle clean
