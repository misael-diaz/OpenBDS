/*
 * source: test_list.c
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
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include "flist_methods.h"

// defines OOP-like interfaces
#define list_t() flist_create_list_t()
#define append_method(i) flist_append_int32_t_method(list, i)

#define NUMEL 65536

__attribute__ ((access (read_only, 1)))
int32_t** create_iter_t (const list_t* list);
int32_t** destroy_iter_t (int32_t* it, size_t size);

int main () {
	// shows how to generate a linked-list

	clock_t tic, toc;
	list_t* list = list_t ();		// creates list<*>


	printf("\n\nappending values to list ... ");

	tic = clock ();
	for (int32_t i = 0; i != NUMEL; ++i)
		append_method (&i);		// appends values to list
	toc = clock ();

	printf("done\n");
        printf("CPU time (millis): %f\n\n\n", 1000.0 *
                ( (double) (toc - tic) ) / CLOCKS_PER_SEC);


	printf("generating random-access iterator ... ");

	tic = clock ();
	int32_t diff = 0;
	int32_t** it = create_iter_t (list);
	// checks for differences between the input and stored data
	for (int32_t i = 0; i != (list -> size); ++i)
		diff += ( *(it[i]) - i );
	toc = clock ();

	printf("done\n");
        printf("CPU time (millis): %f\n\n\n", 1000.0 *
                ( (double) (toc - tic) ) / CLOCKS_PER_SEC);


	printf("[00] test-list-iterator: ");
	if (diff)
		printf("FAIL\n\n\n");
	else
		printf("pass\n\n\n");

	// prints the first and last values in the list
	int *head = list -> head -> node -> data;
	int *tail = list -> tail -> node -> data;
	printf("head: %d\n", *head);
	printf("tail: %d\n", *tail);

	/* frees allocated resources */
	printf("\n\nfreeing allocated resources ... ");

	tic = clock ();
	it = destroy_iter_t (it, list -> size);
	list = flist_list_destructor (list);
	toc = clock ();

	printf("done\n");
        printf("CPU time (millis): %f\n\n\n", 1000.0 *
                ( (double) (toc - tic) ) / CLOCKS_PER_SEC);

	return 0 ;
}


__attribute__ ((access (read_only, 1)))
int32_t** create_iter_t (const list_t* list)	// creates iterator
{
	int32_t** it = (int32_t**) malloc ( list -> size * sizeof(int*) );

	if (it == NULL)
	{
		list = flist_list_destructor (list);
                fprintf (stderr, "memory allocation error: %s\n",
                         strerror (errno) );
                exit(EXIT_FAILURE);
	}

	node_t* node = list -> head -> node;

	size_t i = 0;
	while (node)
	{
		it[i++] = node -> data;
		node = node -> next;
	}


	return it;
}


int32_t** destroy_iter_t (int32_t* it, size_t size)  // destroys iterator
{
	if (it)
	{
		for (size_t i = 0; i != size; ++i)
			it[i] = NULL;

		free (it);
		it = NULL;
	}

	return it;
}


/*
 *
 * TODO:
 * [x] cater appending mixed-type objects to linked-list
 *
 */
