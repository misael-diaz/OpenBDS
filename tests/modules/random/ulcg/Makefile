#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-06-05
#
# Synopsis:
# Builds the programs for testing Linear Congruential Generators.
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

include test-ulcg-inc

all: $(TESTS)


$(TEST_SMALL_LCG_EXE): $(TEST_SMALL_LCG_OBJ)
	$(FC) $(FC_OPT) $(MODULES_INC) $(ULCG_MOD_OBJ)\
	       	$(TEST_SMALL_LCG_OBJ) -o $(TEST_SMALL_LCG_EXE)

$(TEST_SMALL_LCG_OBJ): $(ULCG_MOD_OBJ) $(TEST_SMALL_LCG_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(TEST_SMALL_LCG_SRC) -o\
	       	                          $(TEST_SMALL_LCG_OBJ)


clean:
	rm -f *.mod *.o $(TESTS)
