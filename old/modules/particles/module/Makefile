#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-06-11
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

include particles-inc

all: $(PARTICLES_MODULE_OBJ) $(PARTICLES_SUBMOD_OBJ)

$(PARTICLES_MODULE_OBJ): $(PARTICLES_MODULE_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(PARTICLES_MODULE_SRC) -o\
	                                  $(PARTICLES_MODULE_OBJ)

$(PARTICLES_SUBMOD_OBJ): $(PARTICLES_MODULE_OBJ) $(PARTICLES_SUBMOD_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(PARTICLES_SUBMOD_SRC) -o\
	                                  $(PARTICLES_SUBMOD_OBJ)

clean:
	rm -f *.mod *.smod *.o
