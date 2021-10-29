#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-10-28
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

include make-inc

all: $(ITER_MODULE_OBJ) $(ITER_SUBMOD_OBJ)

$(ITER_MODULE_OBJ): $(ITER_MODULE_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(ITER_MODULE_SRC) -o\
	                                  $(ITER_MODULE_OBJ)

$(ITER_IMPLEMENTS_OBJ): $(ITER_MODULE_OBJ) $(ITER_IMPLEMENTS_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c  $(ITER_IMPLEMENTS_SRC) -o\
	                                   $(ITER_IMPLEMENTS_OBJ)

$(ITER_METHODS_OBJ): $(ITER_MODULE_OBJ) $(ITER_METHODS_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(ITER_METHODS_SRC) -o\
	                                  $(ITER_METHODS_OBJ)

clean:
	rm -f *.mod *.smod *.o submodules/*.mod \
	 submodules/*.smod     submodules/*.o
