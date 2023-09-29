#include <stdio.h>

#include "system/params.h"
#include "system/box/params.h"
#include "system/box/utils.h"

#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
#define LIMIT ( (double) ( __OBDS_LIMIT__ ) )

void test(void);

int main ()
{
  test();
  return 0;
}


void test (void)
{
  prop_t x[NUMEL];

  double* data = &x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    data[i] = 0.0;
  }

  data[0] = -1.0009765625;
  data[1] = -1.0000000000;
  data[2] = -0.5000000000;
  data[3] = +0.0000000000;
  data[4] = +0.5000000000;
  data[5] = +1.0000000000;
  data[6] = +1.0009765625;

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const c = LIMIT;
    data[i] *= c;
  }

  prop_t offset[NUMEL];
  prop_t bitmask[NUMEL];
  pbc(x, offset, bitmask);

  bool failed = false;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    if (data[i] < -LIMIT || data[i] > LIMIT)
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

source: test/system-box/test.c
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
