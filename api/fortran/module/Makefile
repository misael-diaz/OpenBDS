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

all: spheres

configs:
	@$(MAKE) -C config

PRNGs:
	@$(MAKE) -C random

particles: configs
	@$(MAKE) -C particle

systems: particles
	@$(MAKE) -C system

ios: particles
	@$(MAKE) -C io

forces: particles PRNGs
	@$(MAKE) -C force

dynamics: forces
	@$(MAKE) -C dynamic

spheres: ios dynamics systems
	@$(MAKE) -C sphere

clean:
	@$(MAKE) -C config clean
	@$(MAKE) -C random clean
	@$(MAKE) -C particle clean
	@$(MAKE) -C system clean
	@$(MAKE) -C io clean
	@$(MAKE) -C force clean
	@$(MAKE) -C dynamic clean
	@$(MAKE) -C sphere clean
	rm -f mods/*.mod
