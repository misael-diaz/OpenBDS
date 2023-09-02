#include <stddef.h>
#include "util/arrays.h"
#include "system.h"

#define NUMEL NUM_SPHERES

void copy (const double* restrict src, double* restrict dst)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    dst[i] = src[i];
  }
}

void zeros (double* x)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = 0.0;
  }
}

void iota (int64_t* x)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = i;
  }
}

/*

OpenBDS								July 19, 2023

source: arrays.c
author: @misael-diaz

Synopsis:
Implements numpy-like utility methods for arrays.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
