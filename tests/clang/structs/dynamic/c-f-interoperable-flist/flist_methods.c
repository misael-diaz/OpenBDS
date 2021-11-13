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

void** flist_random_access_iterator (list_t* list)
{	// creates a random-access iterator

	// initializes forward iterator
	link_t *head = (list && list -> head)? list -> head: NULL;
	node_t *node = (head && head -> node)? head -> node: NULL;


	size_t numel = 0;
	while (node)
	{	// obtains the number of elements in the list
		node = node -> next;
		++numel;
	}


	size_t sz = numel * sizeof(void*);
	// allocates memory for random-access iterator
	void** iter = (sz)? util_alloc_iter_t (sz): NULL;


	// initializes random-access iterator
	node = (head && head -> node)? head -> node: NULL;
	for (size_t i = 0; i != numel; ++i)
	{
		iter[i] = node -> item -> data;
		node = node -> next;
	}

	return iter;
}


static void flist_append_int32_t (void *vlist, int value)  // append method
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


node_t* flist_create_node_int32_t (int value)	// creates node<*int32_t>
{

	node_t *node = util_alloc_node_t ();
	node -> item = util_alloc_data_t ();
	node -> item -> data = util_alloc_void_t ( sizeof(int) );
	node -> next = NULL;

	int* i = node -> item -> data;
	*i = value;

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
	// initializes forward iterators
	node_t* head = (link -> node)? link -> node: NULL;
	node_t* prev = (head && head -> next)? head -> next: NULL;
	node_t* node = (prev && prev -> next)? prev -> next: NULL;
	node_t* next = (node && node -> next)? node -> next: NULL;
	node_t* tail = (next && next -> next)? next -> next: NULL;

	while (head != NULL)
	{
		head = util_ffree_node_t (head);
		prev = util_ffree_node_t (prev);
		node = util_ffree_node_t (node);
		next = util_ffree_node_t (next);
		// advances forward iterators
		head = tail;
		prev = (head && head -> next)? head -> next: NULL;
		node = (prev && prev -> next)? prev -> next: NULL;
		next = (node && node -> next)? node -> next: NULL;
		tail = (next && next -> next)? next -> next: NULL;
	}

	link -> node = NULL;
	link = util_ffree_link_t (link);
	return link;

}
