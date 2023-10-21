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

all: tests OBDS

utils:
	@$(MAKE) -C util

input-output:
	@$(MAKE) -C io

system-utils:
	@$(MAKE) -C system

particles: utils input-output system-utils
	@$(MAKE) -C particle

tests: particles
	@$(MAKE) -C test

OBDS: particles
	@$(MAKE) -C bds

clean:
	@$(MAKE) -C io clean
	@$(MAKE) -C util clean
	@$(MAKE) -C system clean
	@$(MAKE) -C particle clean
	@$(MAKE) -C test clean
	@$(MAKE) -C bds clean
