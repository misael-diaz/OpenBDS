#!/usr/bin/make
#
# source: Makefile
# author: misael-diaz
# date:   2021-06-10
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

all: $(VECTOR_MODULE_OBJ) $(VECTOR_SUBMOD_OBJ)

$(VECTOR_MODULE_OBJ): $(VECTOR_MODULE_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_MODULE_SRC) -o\
	                                  $(VECTOR_MODULE_OBJ)

$(VECTOR_ERASE_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_ERASE_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_ERASE_SRC) -o\
	                                  $(VECTOR_ERASE_OBJ)

$(VECTOR_INT32_PUSH_BACK_OBJ): $(VECTOR_MODULE_OBJ)\
                               $(VECTOR_INT32_PUSH_BACK_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_INT32_PUSH_BACK_SRC) -o\
	                                  $(VECTOR_INT32_PUSH_BACK_OBJ)

$(VECTOR_INT64_PUSH_BACK_OBJ): $(VECTOR_MODULE_OBJ)\
                               $(VECTOR_INT64_PUSH_BACK_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_INT64_PUSH_BACK_SRC) -o\
	                                  $(VECTOR_INT64_PUSH_BACK_OBJ)

$(VECTOR_REAL64_PUSH_BACK_OBJ): $(VECTOR_MODULE_OBJ)\
                                $(VECTOR_REAL64_PUSH_BACK_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_REAL64_PUSH_BACK_SRC) -o\
	                                  $(VECTOR_REAL64_PUSH_BACK_OBJ)

$(VECTOR_VECTOR_PUSH_BACK_OBJ): $(VECTOR_MODULE_OBJ)\
                                $(VECTOR_VECTOR_PUSH_BACK_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_VECTOR_PUSH_BACK_SRC) -o\
	                                  $(VECTOR_VECTOR_PUSH_BACK_OBJ)

$(VECTOR_POINTER_PUSH_BACK_OBJ): $(VECTOR_MODULE_OBJ)\
                                 $(VECTOR_POINTER_PUSH_BACK_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c \
                                 $(VECTOR_POINTER_PUSH_BACK_SRC) -o\
                                 $(VECTOR_POINTER_PUSH_BACK_OBJ)

$(VECTOR_UTILIT_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_UTILIT_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_UTILIT_SRC) -o\
	                                  $(VECTOR_UTILIT_OBJ)

$(VECTOR_OBLIVI_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_OBLIVI_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_OBLIVI_SRC) -o\
	                                  $(VECTOR_OBLIVI_OBJ)

$(VECTOR_MINT32_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_MINT32_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_MINT32_SRC) -o\
	                                  $(VECTOR_MINT32_OBJ)

$(VECTOR_IINT32_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_IINT32_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_IINT32_SRC) -o\
	                                  $(VECTOR_IINT32_OBJ)

$(VECTOR_MINT64_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_MINT64_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_MINT64_SRC) -o\
	                                  $(VECTOR_MINT64_OBJ)

$(VECTOR_IINT64_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_IINT64_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_IINT64_SRC) -o\
	                                  $(VECTOR_IINT64_OBJ)

$(VECTOR_METR64_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_METR64_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_METR64_SRC) -o\
	                                  $(VECTOR_METR64_OBJ)

$(VECTOR_IMPR64_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_IMPR64_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_IMPR64_SRC) -o\
	                                  $(VECTOR_IMPR64_OBJ)

$(VECTOR_MVECTT_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_MVECTT_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_MVECTT_SRC) -o\
	                                  $(VECTOR_MVECTT_OBJ)

$(VECTOR_IVECTT_OBJ): $(VECTOR_MODULE_OBJ) $(VECTOR_IVECTT_SRC)
	$(FC) $(FC_OPT) $(MODULES_INC) -c $(VECTOR_IVECTT_SRC) -o\
	                                  $(VECTOR_IVECTT_OBJ)

clean:
	rm -f *.mod *.smod *.o submodules/*.mod \
	 submodules/*.smod     submodules/*.o
