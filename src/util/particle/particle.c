#include "system/box/params.h"
#include "system/box/utils.h"
#include "system/params.h"

#define STDC17 201710L
#define TSTEP ( (double) ( __OBDS_TIME_STEP__ ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
#define LENGTH ( (double) ( __OBDS_LENGTH__ ) )
#define ISOTROPIC_RESISTANCE ( FCONF_ISOTROPIC_HYDRODYNAMIC_RESISTANCE_PARTICLE )
#define ISOTROPIC ( (bool) ( __OBDS_ISOTROPIC_HYDRODYNAMIC_RESISTANCE__ ) )
#define ANISOTROPIC ( (bool) ( !(ISOTROPIC) ) )

#if (ISOTROPIC_RESISTANCE == 0x00000001)

// translates the particles by updating the respective component of the position vectors
static void translate (double* __restrict__ x, const double* __restrict__ F_x)
{
#define LINEAR_MOBILITY ( (double) __OBDS_TIME_STEP__ )
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  constexpr double linear_mobility = LINEAR_MOBILITY;
#else
  double const linear_mobility = LINEAR_MOBILITY;
#endif
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += linear_mobility * F_x[i];
  }
}

// translates the particles in the x, y, and z directions
static void translate_isotropic (particle_t* particles)
{
  double* x = &(particles -> x -> data);
  double* y = &(particles -> y -> data);
  double* z = &(particles -> z -> data);
  double* r_x = &(particles -> r_x -> data);
  double* r_y = &(particles -> r_y -> data);
  double* r_z = &(particles -> r_z -> data);
  const double* f_x = &(particles -> f_x -> data);
  const double* f_y = &(particles -> f_y -> data);
  const double* f_z = &(particles -> f_z -> data);
  // updates the position vectors subjected to the periodicity of the system box
  translate(x, f_x);
  translate(y, f_y);
  translate(z, f_z);
  // updates the ``absolute'' position vectors
  translate(r_x, f_x);
  translate(r_y, f_y);
  translate(r_z, f_z);
}

// delegates the task of translating the isotropic resistance particles (or spheres)
void util_particle_translate (particle_t* particles)
{
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  static_assert(ISOTROPIC, "configuration error");
#else
  _Static_assert(ISOTROPIC, "configuration error");
#endif
  translate_isotropic(particles);
}

#else

static void translate (particle_t* particles)
{
  _Static_assert(ANISOTROPIC, "configuration error");
}

static void translate_anisotropic (particle_t* particles)
{
  _Static_assert(ANISOTROPIC, "configuration error");
  translate(particles);
}

void util_particle_translate (particle_t* particles)
{
  _Static_assert(ANISOTROPIC, "unimplemented error");
  translate_anisotropic(particles);
}

#endif

// void util_particle_bruteforce(particles, callback)
//
// Synopsis:
// Uses brute force to compute the resultant (deterministic) forces on all the particles,
// accounts for the periodicity of the system box. Note that the particles positions in
// the neighboring boxes are obtained by applying an offset to the components of the
// position vectors `x', `y', and `z'.
//
// Notes on Performance:
// This is the bottleneck of the Brownian Dynamics Simulator BDS, the time complexity of
// this method is quadratic. We can do better though, a divide and conquer approach might
// be implemented (later) to reach an overall time complexity of O(N log N). Having a
// working application is far more important to me now than optimizing it to the extent
// possible of my current ability.
//
// Parameters:
// particles	particle property placeholder (position, orientation, ids, etc.)
// callback	method that implements the particle force computation

void util_particle_brute_force (particle_t* particles,
				void (*callback) (particle_t* particles,
						  size_t const i,
						  double const offset_x,
						  double const offset_y,
						  double const offset_z))
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

//for (size_t i = 0; i != NUMEL; ++i)
//{
//  double const offset_x = 0;
//  double const offset_y = 0;
//  double const offset_z = 0;
//  callback(particles, i, offset_x, offset_y, offset_z);
//}

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }


  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    callback(particles, i, offset_x, offset_y, offset_z);
  }
}


static void pbcs (particle_t* particles)
{
  prop_t* x = particles -> x;
  prop_t* y = particles -> y;
  prop_t* z = particles -> z;
  prop_t* offset = particles -> tmp;
  prop_t* bitmask = particles -> bitmask;
  pbc(x, offset, bitmask);
  pbc(y, offset, bitmask);
  pbc(z, offset, bitmask);
}

// applies periodic boundary conditions on the particles
void util_particle_pbcs (particle_t* particles)
{
  pbcs(particles);
}

/*

OpenBDS								September 26, 2023

source: util/particle/particle.c
author: @misael-diaz

Synopsis:
Implements utility methods for OBDS particles.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/
