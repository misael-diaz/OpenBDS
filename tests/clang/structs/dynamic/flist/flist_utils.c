/*
 * source: flist_utils.c
 * author: misael-diaz
 * date:   2021-11-04
 *
 *
 * Synopsis:
 * Utils of forward linked-lists.
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
#include "flist_utils.h"


static void* flist_util_alloc_void_t (size_t sz)
{
	// generic memory allocator utility

	void *data = malloc (sz);

	if (data == NULL)
	{
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	return data;
}


static data_t* flist_util_alloc_data_t ()
{
	// allocates memory for data_t object

	data_t *item = (data_t*) malloc ( sizeof(data_t) );

	if (item == NULL)
	{
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	item -> data = NULL;
	return item;
}


static node_t* flist_util_alloc_node_t ()
{
	// allocates memory for a node object

	node_t *node = (node_t*) malloc ( sizeof(node_t) );

	if (node == NULL)
	{
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	node -> item = NULL;
	node -> next = NULL;
	return node;
}


static link_t* flist_util_alloc_link_t ()
{
	// allocates memory for a link object

	link_t *link = (link_t*) malloc ( sizeof(link_t) );

	if (link == NULL)
	{
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	link -> node = NULL;
	return link;
}


static list_t* flist_util_alloc_list_t ()
{
	// allocates memory for a link object

	list_t *list = (list_t*) malloc ( sizeof(list_t) );

	if (list == NULL)
	{
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	list -> self = NULL;
	list -> head = NULL;
	list -> tail = NULL;
	list -> append = NULL;

	return list;
}


static void*   flist_util_ffree_void_t (void *data)
{
	// generic deallocator

	if (data)
	{
		free (data);
		data = NULL;
	}

	return data;
}


static data_t* flist_util_ffree_data_t (data_t *item)
{
	// frees data object from memory

	if (item)
	{
		item -> data = flist_util_ffree_void_t (item -> data);
		free (item);
		item = NULL;
	}

	return item;
}


static node_t* flist_util_ffree_node_t (node_t *node)
{
	// frees node object from memory

	if (node)
	{
		node -> item = flist_util_ffree_data_t (node -> item);
		node -> next = NULL;
		free (node);
		node = NULL;
	}

	return node;
}


static link_t* flist_util_ffree_link_t (link_t *link)
{
	// frees link object from memory

	if (link)
	{
		link -> node = NULL;
		free (link);
		link = NULL;
	}

	return link;
}


static list_t* flist_util_ffree_list_t (list_t *list)
{
	// frees list object from memory

	if (list)
	{
		list -> self = NULL;
		list -> head = flist_util_ffree_link_t (list -> head);
		list -> tail = flist_util_ffree_link_t (list -> tail);
		free(list);
		list = NULL;
	}

	return list;
}


/* creates util namespace instance */
flist_util_namespace const util = {
	flist_util_alloc_void_t,
	flist_util_alloc_data_t,
	flist_util_alloc_node_t,
	flist_util_alloc_link_t,
	flist_util_alloc_list_t,
	flist_util_ffree_void_t,
	flist_util_ffree_data_t,
	flist_util_ffree_node_t,
	flist_util_ffree_link_t,
	flist_util_ffree_list_t
};
