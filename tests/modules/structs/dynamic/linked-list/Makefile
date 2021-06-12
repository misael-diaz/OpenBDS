#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-06-06
#
# Synopsis:
# Builds the programs for testing the LinkedList Class.
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

include LinkedList-test-inc

all: $(TESTS)



$(TEST_LINKING_EXE): $(TEST_LINKING_OBJ)
	$(FC) $(FC_OPT) $(MODULES_INC) $(LINKING_MOD_OBJ)\
                $(TEST_LINKING_OBJ) -o $(TEST_LINKING_EXE)

$(TEST_LINKING_OBJ): $(LINKING_MOD_OBJ) $(TEST_LINKING_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(TEST_LINKING_SRC) -o\
                                          $(TEST_LINKING_OBJ)



$(TEST_LINKED_LIST_EXE): $(SUBM) $(TEST_LINKED_LIST_OBJ)
	$(FC) $(FC_OPT) $(MODULES_INC) $(MODS) $(SUBM)\
	    $(TEST_LINKED_LIST_OBJ) -o $(TEST_LINKED_LIST_EXE)

$(TEST_LINKED_LIST_OBJ): $(MODS) $(TEST_LINKED_LIST_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(TEST_LINKED_LIST_SRC) -o\
	       	                          $(TEST_LINKED_LIST_OBJ)


clean:
	rm -f *.mod *.o $(TESTS)
