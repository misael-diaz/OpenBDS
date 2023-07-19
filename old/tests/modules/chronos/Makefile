#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-05-31
#
# Synopsis:
# Builds the programs for testing the timer classes defined in chronos.
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

include chronos-test-inc

all: $(TESTS)


$(TEST_CHRONOM_EXE): $(CHRONOS_SUB_OBJ) $(TEST_CHRONOM_OBJ)
	$(FC) $(FC_OPT) $(MODULES_INC) $(SMODS) $(TEST_CHRONOM_OBJ) -o\
		                                $(TEST_CHRONOM_EXE)

$(TEST_CHRONOM_OBJ): $(CHRONOS_MOD_OBJ) $(TEST_CHRONOM_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(TEST_CHRONOM_SRC) -o\
	       	                          $(TEST_CHRONOM_OBJ)


clean:
	rm -f *.mod *.o $(TESTS)
