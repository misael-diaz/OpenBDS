#ifndef GUARD_LIST_T_H
#define GUARD_LIST_T_H
/*
 * source: list_t.h
 * author: misael-diaz
 * date:   2021-11-02
 *
 *
 * Synopsis:
 * Header for list type `list_t'.
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

#include "link_t.h"

typedef struct {
	void*   self;
	link_t* head;
	link_t* tail;
	void (*append) (void *list, int value);
} list_t;

#endif
