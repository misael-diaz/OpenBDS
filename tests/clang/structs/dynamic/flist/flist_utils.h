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

typedef struct {
	void*   (*const alloc_void_t) (size_t);
	data_t* (*const alloc_data_t) (void);
	node_t* (*const alloc_node_t) (void);
	link_t* (*const alloc_link_t) (void);
	void*   (*const ffree_void_t) (void*);
	data_t* (*const ffree_data_t) (data_t*);
	node_t* (*const ffree_node_t) (node_t*);
	link_t* (*const ffree_link_t) (link_t*);
	list_t* (*const ffree_list_t) (list_t*);
} util_namespace;

extern util_namespace const util;

//void* util_alloc_void_t (size_t);  // generic memory allocator

//data_t* util_alloc_data_t (); 	// allocates memory for a data_t object
//node_t* util_alloc_node_t ();	// allocates memory for a node object
//link_t* util_alloc_link_t ();	// allocates memory for a link object

//void*   util_ffree_void_t (void*);	// frees <..> object from memory
//data_t* util_ffree_data_t (data_t*);	// frees data object from memory
//node_t* util_ffree_node_t (node_t*);	// frees node object from memory
//link_t* util_ffree_link_t (link_t*);	// frees link object from memory
//list_t* util_ffree_list_t (list_t*);	// frees list object from memory

#endif
