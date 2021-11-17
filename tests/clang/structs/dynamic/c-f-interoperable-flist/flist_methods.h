#ifndef GUARD_FLIST_METHODS_H
#define GUARD_FLIST_METHODS_H
/*
 * source: flist_methods.h
 * author: misael-diaz
 * date:   2021-11-04
 *
 *
 * Synopsis:
 * Header for the methods of forward linked-lists.
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

#include "flist_utils.h"

void** flist_random_access_iterator (list_t*);	// creates iterator

__attribute__ ((access (read_only, 1)))
node_t* flist_create_node_int32_t (const int*);// creates node<*int32_t>
link_t* flist_create_link_t ();		// creates `empty' link<>
list_t* flist_create_list_t ();		// creates `empty' list<>
link_t* flist_link_destructor (link_t*);// destroys linked-nodes
list_t* flist_list_destructor (list_t*);// destroys linked-list
void    flist_alist_destructor (list_t*);

#endif
