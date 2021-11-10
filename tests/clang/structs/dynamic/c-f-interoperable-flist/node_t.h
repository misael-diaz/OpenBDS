#ifndef GUARD_NODE_T_H
#define GUARD_NODE_T_H
/*
 * source: node_t.h
 * author: misael-diaz
 * date:   2021-11-02
 *
 *
 * Synopsis:
 * Header for node type `node_t'.
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

#include "data_t.h"

typedef struct {
	data_t* item;
	void*   next;
} node_t;

#endif
