#ifndef GUARD_FLIST_METHODS_H
#define GUARD_FLIST_METHODS_H
/*
 * source: flist_methods.h
 * author: misael-diaz
 * date:   2021-11-04
 *
 *
 * Synopsis:
 * Implements methods for forward linked-lists.
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

/* iterator */

__attribute__ ((access (read_only, 1)))
iter_t* flist_create_iter_t (const list_t*);	// creates iterator

/* constructors */

list_t* flist_create_list_t ();			// creates list<*>
list_t* flist_create_list_int32_t (int32_t*);	// creates list<int32_t>
list_t* flist_create_list_int64_t (int64_t*);	// creates list<int64_t>

/* append methods */

__attribute__ ((access (read_only, 2)))
void flist_append_int32_t_method (list_t*, const int32_t*);
__attribute__ ((access (read_only, 2)))
void flist_append_int64_t_method (list_t*, const int64_t*);

/* destructors */

iter_t* flist_destroy_iter_t (iter_t*);		// destroys iterator
list_t* flist_list_destructor (list_t*);	// destroys linked-list

#endif
