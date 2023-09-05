#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#include "system.h"
#include "system/box.h"

#define NUMEL NUM_PARTICLES

void test(void);

int main ()
{
  test();
  return 0;
}


void test (void)
{
  double x[NUMEL];

  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = 0.0;
  }

  x[0] = -1.0009765625;
  x[1] = -1.0000000000;
  x[2] = -0.5000000000;
  x[3] = +0.0000000000;
  x[4] = +0.5000000000;
  x[5] = +1.0000000000;
  x[6] = +1.0009765625;

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const c = LIMIT;
    x[i] *= c;
  }

  double offset[NUMEL];
  double bitmask[NUMEL];
  pbc(x, offset, bitmask);

  bool failed = false;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (x[i] < -LIMIT || x[i] > LIMIT)
    {
      failed = true;
      break;
    }
  }

  printf("test-system-box[0]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


/*

OpenBDS							September 05, 2023

source: test.c
author: @misael-diaz

Synopsis:
Tests the pbc() method that applies periodic boundary conditions.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
