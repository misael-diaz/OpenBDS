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

#include <stddef.h>
#include "flist_methods.h"

extern flist_util_namespace const util;	// imports util namespace


/* implementation */


static link_t* create_link_t ()		// creates link<*node_t>
{
        link_t *link = util.alloc_link_t ();
	return link;
}


__attribute__ ((access (read_only, 1)))
static node_t* create_node_int32_t (const int* value)
{
	/* creates node<*int32_t> */

	node_t *node = util.alloc_node_t ();
	node -> item = util.alloc_data_t ();
	node -> item -> data = util.alloc_void_t ( sizeof(int) );
	node -> next = NULL;

	int* i = node -> item -> data;
	*i = *value;

	return node ;
}


/* methods */


__attribute__ ((access (read_only, 2)))
static void append_int32_t (void* vlist, const int* value)	// append
{
	list_t *list = vlist;
	if (list -> head -> node == NULL)
	{
		list -> head -> node = create_node_int32_t (value);
		list -> tail -> node = list -> head -> node;
	}
	else
	{
		list -> tail -> node -> next = create_node_int32_t (value);
		list -> tail -> node = list -> tail -> node -> next;
	}
}


static link_t* link_destructor (link_t* link)	// destroys linked nodes
{
	node_t* node = (link && link -> node)? link -> node: NULL;
	node_t* next = (node && node -> next)? node -> next: NULL;

	while (node)
	{
		node = util.ffree_node_t (node);
		node = next;
		next = (next && next -> next)? next -> next: NULL;
	}

	return util.ffree_link_t (link);
}


/* constructor */


list_t* flist_create_list_t ()			// creates list<*link_t>
{
	list_t *list = util.alloc_list_t ();
	list -> self = list;
	list -> head = create_link_t ();
	list -> tail = create_link_t ();
	list -> append = append_int32_t;

	return list;
}


/* destructor */


list_t* flist_list_destructor (list_t* list)	// destroys linked-list
{
	link_t *head = (list && list -> head)? (list -> head): NULL;
	node_t *node = (head && head -> node)? (head -> node): NULL;

	if (head && node)
	{
		list -> head = link_destructor   (list -> head);
		list -> tail = util.ffree_link_t (list -> tail);
	}

	return util.ffree_list_t (list);
}
