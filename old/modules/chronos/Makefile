#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-05-31
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

include chronos-inc

all: $(CHRONOS_MODULE_OBJ) $(CHRONOS_SUBMOD_OBJ)

$(CHRONOS_MODULE_OBJ): $(CHRONOS_MODULE_SRC)
	$(FC) $(FC_OPT) -c $(CHRONOS_MODULE_SRC) -o $(CHRONOS_MODULE_OBJ)

$(CHRONOS_SUBMOD_OBJ): $(CHRONOS_MODULE_OBJ) $(CHRONOS_SUBMOD_SRC)
	$(FC) $(FC_OPT) -c $(CHRONOS_SUBMOD_SRC) -o $(CHRONOS_SUBMOD_OBJ)

clean:
	rm -f *.mod *.smod *.o
