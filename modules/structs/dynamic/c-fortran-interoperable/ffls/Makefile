#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-11-02
#
# Synopsis:
# Builds the programs for testing the forward linked-list [flist].
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

include make-inc

all: $(FFLS_MODULE_OBJ) $(FFLS_SUBMOD_OBJ)

$(FFLS_MODULE_OBJ): $(FFLS_MODULE_SRC)
	$(FC) $(FC_OPT) -c $(FFLS_MODULE_SRC) -o $(FFLS_MODULE_OBJ)

$(FFLS_FCTORS_OBJ): $(FFLS_MODULE_OBJ) $(FFLS_FCTORS_SRC)
	$(FC) $(FC_OPT) -c $(FFLS_FCTORS_SRC) -o $(FFLS_FCTORS_OBJ)

$(FFLS_FDTORS_OBJ): $(FFLS_MODULE_OBJ) $(FFLS_FDTORS_SRC)
	$(FC) $(FC_OPT) -c $(FFLS_FDTORS_SRC) -o $(FFLS_FDTORS_OBJ)

$(FFLS_METHODS_OBJ): $(FFLS_MODULE_OBJ) $(FFLS_METHODS_SRC)
	$(FC) $(FC_OPT) -c $(FFLS_METHODS_SRC) -o $(FFLS_METHODS_OBJ)

clean:
	rm -f *.mod *.smod *.o submodules/*.mod submodules/*.smod\
                submodules/*.o
