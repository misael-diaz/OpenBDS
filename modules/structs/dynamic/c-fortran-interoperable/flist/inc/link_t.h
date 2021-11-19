#ifndef GUARD_LINK_T_H
#define GUARD_LINK_T_H
/*
 * source: link_t.h
 * author: misael-diaz
 * date:   2021-11-02
 *
 *
 * Synopsis:
 * Header for link type `link_t'.
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

#include "node_t.h"

typedef struct {
	node_t* node;
} link_t;

#endif
