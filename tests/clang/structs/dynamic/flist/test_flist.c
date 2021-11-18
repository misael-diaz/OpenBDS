/*
 * source: test_flist.c
 * author: misael-diaz
 * date:   2021-11-04
 *
 *
 * Synopsis:
 * Test the forward linked-list [flist].
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
#include "flist_methods.h"

// defines OOP-like interfaces
#define list_t(i) flist_create_list_t()
#define append_method(i) append_int32_t(list->self, i)

#define NUMEL 65536

int main () {
	// shows how to generate a linked-list

	list_t* list = list_t ();		// creates list<*>

	for (int i = 0; i != NUMEL; ++i) {
		list -> append_method (&i);	// appends values to list
	}

//	Complains:
//	long int ii = 0;
//	list -> append_int64_t (list->self, &ii);

	// prints the first and last values in the list
	int *head = list -> head -> node -> item -> data;
	int *tail = list -> tail -> node -> item -> data;
	printf("head: %d\n", *head);
	printf("tail: %d\n", *tail);

	/* frees allocated resources */
	list = flist_list_destructor (list);

	return 0 ;
}

/*
 *
 * TODO:
 * [x] cater appending mixed-type objects to linked-list
 *
 */
