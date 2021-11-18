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

#include "flist.h"

typedef struct {
	void*   (*const alloc_void_t) (size_t);	// allocates <::> object
	data_t* (*const alloc_data_t) (void);	// allocates data object
	node_t* (*const alloc_node_t) (void);	// allocates node object
	link_t* (*const alloc_link_t) (void);	// allocates link object
	list_t* (*const alloc_list_t) (void);	// allocates list object
	void*   (*const ffree_void_t) (void*);	// frees <::> object
	data_t* (*const ffree_data_t) (data_t*);// frees data object
	node_t* (*const ffree_node_t) (node_t*);// frees node object
	link_t* (*const ffree_link_t) (link_t*);// frees link object
	list_t* (*const ffree_list_t) (list_t*);// frees list object
} flist_util_namespace;

#endif
