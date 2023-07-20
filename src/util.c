#include <math.h>
#include "util.h"


bool sign (double const x)
{
  bool const ret = signbit(x);
  return ret;
}


void zeros (size_t const size, double* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    x[i] = 0.0;
  }
}


void iota (size_t const size, double* x)
{
  for (size_t i = 0; i != size; ++i)
  {
    double const id = i;
    x[i] = id;
  }
}


/*

OpenBDS								July 19, 2023

source: util.c
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
