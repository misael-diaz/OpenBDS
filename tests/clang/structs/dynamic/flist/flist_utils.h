#ifndef GUARD_FLIST_UTILS_H
#define GUARD_FLIST_UTILS_H
/*
 * source: flist_utils.h
 * author: misael-diaz
 * date:   2021-11-04
 *
 *
 * Synopsis:
 * Header utils of the forward linked-list.
 *
 *
 * Copyright (c) 2021 Misael Diaz-Maldonado
 *
 * This file is released under the GNU General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 *
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "flist.h"

void* util_alloc_void_t (size_t);  // generic memory allocator

data_t* util_alloc_data_t (); 	// allocates memory for a data_t object
node_t* util_alloc_node_t ();	// allocates memory for a node object
link_t* util_alloc_link_t ();	// allocates memory for a link object

int*    util_ffree_ii32_t (int*);	// frees 32-bit int  from memory
data_t* util_ffree_data_t (data_t*);	// frees data object from memory
node_t* util_ffree_node_t (node_t*);	// frees node object from memory
link_t* util_ffree_link_t (link_t*);	// frees link object from memory
list_t* util_ffree_list_t (list_t*);	// frees list object from memory

#endif
