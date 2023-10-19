#!/usr/bin/make
#
# OpenBDS						July 19, 2023
#
# source: test/Makefile
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

all: PRNGs util-system-box spheres

PRNGs:
	@$(MAKE) -C random

util-system-box:
	@$(MAKE) -C system-box

spheres:
	@$(MAKE) -C particle-sphere

clean:
	@$(MAKE) -C random clean
	@$(MAKE) -C system-box clean
	@$(MAKE) -C particle-sphere clean
