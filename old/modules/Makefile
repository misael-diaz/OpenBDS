#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-06-02
#
# Synopsis:
# Builds the OpenBDS modules
#
#
# Copyright (C) 2021 Misael Diaz-Maldonado
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


all: utils_mod math_mod chronos_mod structs_mod random_mod particles_mod

utils_mod:
	@$(MAKE) -C utils
math_mod: utils_mod
	@$(MAKE) -C math
chronos_mod:
	@$(MAKE) -C chronos
structs_mod: utils_mod
	@$(MAKE) -C structs
random_mod:
	@$(MAKE) -C random
particles_mod:
	@$(MAKE) -C particles

clean:
	@$(MAKE) -C utils     clean
	@$(MAKE) -C math      clean
	@$(MAKE) -C chronos   clean
	@$(MAKE) -C structs   clean
	@$(MAKE) -C random    clean
	@$(MAKE) -C particles clean
