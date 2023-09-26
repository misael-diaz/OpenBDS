#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include "bds/types.h"
#include "bds/params.h"
#include "system/params.h"

#define SUCCESS ( (int) ( __OBDS_SUCCESS__ ) )
#define FAILURE ( (int) ( __OBDS_FAILURE__ ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )

// logs the particle properties (position, orientation, id, etc.) to a plain text file
int io_logger_log (const particle_t* particles, const char* log)
{
  char txt[80];
  char tmp[80];
  strcpy(txt, log);
  strcat(txt, ".txt");
  strcpy(tmp, log);
  strcat(tmp, ".tmp-XXXXXX");
  int fd = mkstemp(tmp);
  if (fd == FAILURE)
  {
    fprintf(stderr, "io.logger.log(): IO ERROR with file %s: %s\n", tmp, strerror(errno));
    return FAILURE;
  }

  FILE* file = fdopen(fd, "w+");
  if (file == NULL)
  {
    fprintf(stderr, "io.logger.log(): IO ERROR with file %s: %s\n", tmp, strerror(errno));
    return FAILURE;
  }

  const double* x = &(particles -> x -> data);
  const double* y = &(particles -> y -> data);
  const double* z = &(particles -> z -> data);
  const double* r_x = &(particles -> r_x -> data);
  const double* r_y = &(particles -> r_y -> data);
  const double* r_z = &(particles -> r_z -> data);
  const double* a_x = &(particles -> a_x -> data);
  const double* a_y = &(particles -> a_y -> data);
  const double* a_z = &(particles -> a_z -> data);
  const double* d_x = &(particles -> d_x -> data);
  const double* d_y = &(particles -> d_y -> data);
  const double* d_z = &(particles -> d_z -> data);
  const double* f_x = &(particles -> f_x -> data);
  const double* f_y = &(particles -> f_y -> data);
  const double* f_z = &(particles -> f_z -> data);
  const double* t_x = &(particles -> t_x -> data);
  const double* t_y = &(particles -> t_y -> data);
  const double* t_z = &(particles -> t_z -> data);
  const uint64_t* pid = &(particles -> id -> bin);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    const char fmt [] = "%+.16e %+.16e %+.16e "
			"%+.16e %+.16e %+.16e "
			"%+.16e %+.16e %+.16e "
			"%+.16e %+.16e %+.16e "
			"%+.16e %+.16e %+.16e "
			"%+.16e %+.16e %+.16e "
			"%" PRIu64 "\n";
    fprintf(file, fmt,   x[i],   y[i],   z[i],
		       r_x[i], r_y[i], r_z[i],
		       a_x[i], a_y[i], a_z[i],
		       d_x[i], d_y[i], d_z[i],
		       f_x[i], f_y[i], f_z[i],
		       t_x[i], t_y[i], t_z[i],
		       pid[i]);
  }

  fclose(file);

  if (rename(tmp, txt) == FAILURE)
  {
    fprintf(stderr, "io.logger.log(): ERROR: %s\n", strerror(errno));
    return FAILURE;
  }

  return SUCCESS;
}

/*

OpenBDS								September 26, 2023

source: io/logger/logger.c
author: @misael-diaz

Synopsis:
Implements methods that perform Input-Output IO operations.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
