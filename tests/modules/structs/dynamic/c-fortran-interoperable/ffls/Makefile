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

include FFList-test-inc

all: $(TESTS)

$(TEST_LIST_EXE): $(TEST_LIST_O)
	$(CC) $(CC_OPT) $(LIST_OBJ) $(TEST_LIST_O) -o $(TEST_LIST_EXE)

$(TEST_LIST_O): $(LIST_OBJ) $(TEST_LIST_C)
	$(CC) $(CC_OPT) -I$(LIST)/inc -c $(TEST_LIST_C) -o $(TEST_LIST_O)

$(TEST_FFLS_EXE): $(LIST_OBJ) $(SUBM) $(TEST_FFLS_OBJ)
	$(FC) $(FC_OPT) $(MODS) $(SUBM) $(LIST_OBJ) $(TEST_FFLS_OBJ)\
                                                 -o $(TEST_FFLS_EXE)

$(TEST_FFLS_OBJ): $(MODS) $(TEST_FFLS_SRC)
	$(FC) $(FC_OPT) $(INCS) -c $(TEST_FFLS_SRC) -o $(TEST_FFLS_OBJ)

clean:
	rm -f *.mod *.smod *.o $(TESTS)
