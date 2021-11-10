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

#include "flist_methods.h"

// defines OOP-like interfaces
#define list_t() flist_create_list_t()
#define append_method(i) append(list->self, i)

#define NUMEL 65536

int main () {
	// shows how to generate a linked-list

	list_t* list = list_t ();		// creates list object

	for (int i = 0; i != NUMEL; ++i) {
		list -> append_method (i);	// appends values to list
	}

	// prints the first and last values in the list
	int *head = list -> head -> node -> item -> data;
	int *tail = list -> tail -> node -> item -> data;
	printf("head: %d\n", *head);
	printf("tail: %d\n", *tail);


	/* creates random-access iterator */
	void** iter = (void**) malloc ( NUMEL * sizeof(void*) );
	for (int i = 0; i != NUMEL; ++i)
		iter[i] = NULL;


	int i = 0;
	node_t* node = NULL;
	node = list -> head -> node;
	while (node)
	{	// populates random-access iterator
		iter[i++] = node -> item -> data;
		node = node -> next;
	}


	int diff = 0;
	for (int i = 0; i != NUMEL; ++i)
		// computes differences between input and contained data
		diff += ( *( (int*) iter[i] ) - i );


	printf("[00] test-list-iterator: ");
	if (diff != 0)
		printf("FAIL\n");
	else
		printf("pass\n");


	/* frees allocated resources */
	list = flist_list_destructor (list);
	free (iter);

	return 0 ;
}

/*
 * TODO:
 * [x] implement memory management utils
 * [x] implement the append method
 *
 */
