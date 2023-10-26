#define _XOPEN_SOURCE 700
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
static int property_logger (const particle_t* particles, const char* log)
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
//position vectors (subject to periodic boundary conditions)
  const prop_t* prop_x = particles -> x;
  const prop_t* prop_y = particles -> y;
  const prop_t* prop_z = particles -> z;
//position vectors
  const prop_t* prop_r_x = particles -> r_x;
  const prop_t* prop_r_y = particles -> r_y;
  const prop_t* prop_r_z = particles -> r_z;
//Euler angle vectors
  const prop_t* prop_a_x = particles -> a_x;
  const prop_t* prop_a_y = particles -> a_y;
  const prop_t* prop_a_z = particles -> a_z;
//orientation vectors (or directors)
  const prop_t* prop_d_x = particles -> d_x;
  const prop_t* prop_d_y = particles -> d_y;
  const prop_t* prop_d_z = particles -> d_z;
//force vectors
  const prop_t* prop_f_x = particles -> f_x;
  const prop_t* prop_f_y = particles -> f_y;
  const prop_t* prop_f_z = particles -> f_z;
//torque vectors
  const prop_t* prop_t_x = particles -> t_x;
  const prop_t* prop_t_y = particles -> t_y;
  const prop_t* prop_t_z = particles -> t_z;
//particle identifier IDs
  const prop_t* prop_id  = particles -> id;

  const double* x = &(prop_x[0].data);
  const double* y = &(prop_y[0].data);
  const double* z = &(prop_z[0].data);
  const double* r_x = &(prop_r_x[0].data);
  const double* r_y = &(prop_r_y[0].data);
  const double* r_z = &(prop_r_z[0].data);
  const double* a_x = &(prop_a_x[0].data);
  const double* a_y = &(prop_a_y[0].data);
  const double* a_z = &(prop_a_z[0].data);
  const double* d_x = &(prop_d_x[0].data);
  const double* d_y = &(prop_d_y[0].data);
  const double* d_z = &(prop_d_z[0].data);
  const double* f_x = &(prop_f_x[0].data);
  const double* f_y = &(prop_f_y[0].data);
  const double* f_z = &(prop_f_z[0].data);
  const double* t_x = &(prop_t_x[0].data);
  const double* t_y = &(prop_t_y[0].data);
  const double* t_z = &(prop_t_z[0].data);
  const uint64_t* pid = &(prop_id[0].bin);
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

// user-interface to the logger
int io_logger_log (const particle_t* particles, const char* log)
{
  return property_logger(particles, log);
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
