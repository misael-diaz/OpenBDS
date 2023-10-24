#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: api/fortran/module/Makefile
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

all: constants PRNGs particles spheres

constants:
	@$(MAKE) -C constant

PRNGs:
	@$(MAKE) -C random

particles: constants PRNGs
	@$(MAKE) -C particle

spheres: particles
	@$(MAKE) -C sphere

clean:
	@$(MAKE) -C constant clean
	@$(MAKE) -C random clean
	@$(MAKE) -C particle clean
	@$(MAKE) -C sphere clean
