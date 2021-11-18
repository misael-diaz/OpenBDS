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

#include "flist_utils.h"

static void* util_alloc_void_t (size_t sz)  // generic memory allocator utility
{

	void *data = malloc (sz);

	if (data == NULL) {
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	return data;

}


static data_t* util_alloc_data_t () 	// allocates memory for data_t object
{
	data_t *item = (data_t*) malloc ( sizeof(data_t) );

	if (item == NULL) {
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	item -> data = NULL;
	return item;

}


static node_t* util_alloc_node_t ()	// allocates memory for a node object
{

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


static link_t* util_alloc_link_t ()	// allocates memory for a link object
{

	link_t *link = (link_t*) malloc ( sizeof(link_t) );

	if (link == NULL) {
		fprintf (stderr, "memory allocation error: %s\n",
			 strerror (errno) );
		exit(EXIT_FAILURE);
	}

	link -> node = NULL;
	return link;

}

static void*   util_ffree_void_t (void *data)	  // generic deallocator
{

	if (data)
	{
		free (data);
		data = NULL;
	}

	return data;

}


static data_t* util_ffree_data_t (data_t *item)  // frees data object from memory
{

	if (item)
	{
		item -> data = util_ffree_void_t (item -> data);
		free (item);
		item = NULL;
	}

	return item;

}


static node_t* util_ffree_node_t (node_t *node)  // frees node object from memory
{

	if (node)
	{
		node -> item = util_ffree_data_t (node -> item);
		node -> next = NULL;
		free (node);
		node = NULL;
	}

	return node;

}


static link_t* util_ffree_link_t (link_t *link)  // frees link object from memory
{
	if (link)
	{
		link -> node = NULL;
		free (link);
		link = NULL;
	}

	return link;
}


static list_t* util_ffree_list_t (list_t *list)  // frees list object from memory
{

	if (list)
	{
		list -> self = NULL;
		list -> head = util_ffree_link_t (list -> head);
		list -> tail = util_ffree_link_t (list -> tail);
		free(list);
		list = NULL;
	}

	return list;
}


util_namespace const util = {	/* creates namespace instance */
	util_alloc_void_t,
	util_alloc_data_t,
	util_alloc_node_t,
	util_alloc_link_t,
	util_ffree_void_t,
	util_ffree_data_t,
	util_ffree_node_t,
	util_ffree_link_t,
	util_ffree_list_t
};
