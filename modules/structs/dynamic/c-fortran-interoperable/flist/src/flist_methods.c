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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "flist_methods.h"

extern flist_util_namespace const util;	// imports util namespace


/* implementation */


__attribute__ ((access (read_only, 1)))
iter_t* flist_create_iter_t (const list_t* list)
{
	iter_t *iter = util.alloc_iter_t (list -> size);
	link_t* head = (list && list -> head)? list -> head: NULL;
	node_t* node = (head && head -> node)? head -> node: NULL;
	data_t* item = NULL;

	size_t i = 0;
	while (node)
	{
		item = node -> item;
		(iter -> data)[i++] = item -> data;
		node = (node -> next)? node -> next: NULL;
	}

	return iter;
}


iter_t* flist_destroy_iter_t (iter_t* iter)
{
	return util.ffree_iter_t (iter);
}


static void is_empty (list_t* list, size_t id)
{
	/* sets error message field of an empty list<*> */

	char e1[] = "list<int32_t>::type-error: "
		"container of 32-bit integers";
	char e2[] = "list<int64_t>::type-error: "
		"container of 64-bit integers";
	char uxerr[] = "list<*>::unexpected-error";

	if (list -> id == 0)
	{
		list -> errmsg = (char*) util.alloc_void_t ( sizeof(e1) );
		switch (id)
		{
			case 1:
			{
				strcpy (list -> errmsg, e1);
				list -> id = 1;
				break;
			}
			case 2:
			{
				strcpy (list -> errmsg, e2);
				list -> id = 2;
				break;
			}
			default:
			{
				list = flist_list_destructor (list);
				fprintf(stderr, "%s\n", uxerr);
				exit(EXIT_FAILURE);
			}

		}
	}
}


static void is_list_int32_t (list_t *list)
{
	/* checks if list is list<int32_t> */

	if (list -> id != 1)
	{
		fprintf(stderr, "%s\n", list -> errmsg);
		list = flist_list_destructor (list);
		exit(EXIT_FAILURE);
	}
}


static void is_list_int64_t (list_t *list)
{
	/* checks if list is list<int64_t> */

	if (list -> id != 2)
	{
		fprintf(stderr, "%s\n", list -> errmsg);
		list = flist_list_destructor (list);
		exit(EXIT_FAILURE);
	}
}


static link_t* create_link_t ()		// creates link<*node_t>
{
        link_t *link = util.alloc_link_t ();
	return link;
}


__attribute__ ((access (read_only, 1)))
static node_t* create_node_int32_t (const int32_t* value)
{
	/* creates node<int32_t> */

	node_t *node = util.alloc_node_t ();
	node -> item = util.alloc_data_t ();
	node -> item -> data = util.alloc_void_t ( sizeof(int32_t) );
	node -> next = NULL;

	int32_t* i = node -> item -> data;
	*i = *value;

	return node ;
}


__attribute__ ((access (read_only, 1)))
static node_t* create_node_int64_t (const int64_t* value)
{
	/* creates node<int64_t> */

	node_t *node = util.alloc_node_t ();
	node -> item = util.alloc_data_t ();
	node -> item -> data = util.alloc_void_t ( sizeof(int64_t) );
	node -> next = NULL;

	int64_t* i = node -> item -> data;
	*i = *value;

	return node ;
}


/* methods */


__attribute__ ((access (read_only, 2)))
void flist_append_int32_t_method (list_t *list, const int32_t* i)
{
	/* appends to list<int32_t> */

	size_t id = 1;
	is_empty (list, id);
	is_list_int32_t (list);
	if (list -> head -> node == NULL)
	{
		list -> head -> node = create_node_int32_t (i);
		list -> tail -> node = list -> head -> node;
	}
	else
	{
		list -> tail -> node -> next = create_node_int32_t (i);
		list -> tail -> node = list -> tail -> node -> next;
	}
	++(list -> size);
}


__attribute__ ((access (read_only, 2)))
void flist_append_int64_t_method (list_t *list, const int64_t* i)
{
	/* appends to list<int64_t> */

	size_t id = 2;
	is_empty (list, id);
	is_list_int64_t (list);
	if (list -> head -> node == NULL)
	{
		list -> head -> node = create_node_int64_t (i);
		list -> tail -> node = list -> head -> node;
	}
	else
	{
		list -> tail -> node -> next = create_node_int64_t (i);
		list -> tail -> node = list -> tail -> node -> next;
	}
	++(list -> size);
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


/* constructors */


list_t* flist_create_list_t ()	// default constructor: creates list<*>
{
	list_t *list = util.alloc_list_t ();
	list -> head = create_link_t ();
	list -> tail = create_link_t ();
	list -> errmsg = NULL;
	list -> size = 0;
	list -> id = 0;

	return list;
}


list_t* flist_create_list_int32_t (int32_t* id)	// creates list<int32_t>
{
	list_t *list = util.alloc_list_t ();
	list -> head = create_link_t ();
	list -> tail = create_link_t ();

	char errmsg[] = "list<int32_t>::type-error: "
		"container of 32-bit integers";
	list -> errmsg = (char*) util.alloc_void_t ( sizeof(errmsg) );
	strcpy (list -> errmsg, errmsg);

	list -> size = 0;
	list -> id = (id)? 1: 1;

	return list;
}


list_t* flist_create_list_int64_t (int64_t* id)	// creates list<int64_t>
{
	list_t *list = util.alloc_list_t ();
	list -> head = create_link_t ();
	list -> tail = create_link_t ();

	char errmsg[] = "list<int64_t>::type-error: "
		"container of 64-bit integers";
	list -> errmsg = (char*) util.alloc_void_t ( sizeof(errmsg) );
	strcpy (list -> errmsg, errmsg);

	list -> size = 0;
	list -> id = (id)? 2: 2;

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
