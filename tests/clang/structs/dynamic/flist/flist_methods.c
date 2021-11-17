/*
 * source: flist_methods.c
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

#include "flist_methods.h"

__attribute__ ((access (read_only, 2)))
static void flist_append_int32_t (void* vlist, const int* value)
// append method
{
	list_t *list = vlist;
	if (list -> head -> node == NULL) {

		list -> head -> node = flist_create_node_int32_t (value);
		list -> tail -> node = list -> head -> node;

	} else {

		list -> tail -> node -> next =
			flist_create_node_int32_t (value);

		list -> tail -> node = list -> tail -> node -> next;

	}

}


list_t* flist_create_list_t ()			// creates empty list<>
{
	list_t *list = (list_t*) malloc ( sizeof(list_t) );
	if (list == NULL) {
                fprintf (stderr, "memory allocation error: %s\n",
                         strerror (errno) );
                exit(EXIT_FAILURE);
	}

	list -> self = list;
	list -> head = flist_create_link_t ();
	list -> tail = flist_create_link_t ();
	list -> append = flist_append_int32_t;
	return list;
}

__attribute__ ((access (read_only, 1)))
node_t* flist_create_node_int32_t (const int* value)
// creates node<*int32_t>
{

	node_t *node = util_alloc_node_t ();
	node -> item = util_alloc_data_t ();
	node -> item -> data = util_alloc_void_t ( sizeof(int) );
	node -> next = NULL;

	int* i = node -> item -> data;
	*i = *value;

	return node ;
}


link_t* flist_create_link_t ()			// creates link<>
{
        link_t *link = util_alloc_link_t ();
	return link;
}


list_t* flist_list_destructor (list_t* list)	// destroys linked-list obj
{
	link_t *head = (list && list -> head)? (list -> head): NULL;
	node_t *node = (head && head -> node)? (head -> node): NULL;

	if (head != NULL && node != NULL)
	{
		list -> head = flist_link_destructor (list -> head);
		list -> tail -> node = NULL;
		list -> tail = util_ffree_link_t (list -> tail);
	}

	return util_ffree_list_t (list);
}


link_t* flist_link_destructor (link_t* link)	// destroys linked-nodes
{
	node_t* node = (link && link -> node)? link -> node: NULL;
	node_t* next = (node && node -> next)? node -> next: NULL;

	while (node)
	{
		node = util_ffree_node_t (node);
		node = next;
		next = (next && next -> next)? next -> next: NULL;
	}

	return util_ffree_link_t (link);
}
