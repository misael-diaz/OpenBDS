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

all: $(OBJS)

$(FLIST_METHODS_O): $(FLIST_UTILS_O) $(FLIST_METHODS_H) $(FLIST_METHODS_C)
	$(CC) $(CC_OPT) -Iinc -c $(FLIST_METHODS_C) -o $(FLIST_METHODS_O)

$(FLIST_UTILS_O): $(TYPES_H) $(FLIST_UTILS_H) $(FLIST_UTILS_C)
	$(CC) $(CC_OPT) -Iinc -c $(FLIST_UTILS_C) -o $(FLIST_UTILS_O)

clean:
	rm -f obj/*.o
