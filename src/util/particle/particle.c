#include "system/box/params.h"
#include "system/box/utils.h"
#include "system/params.h"
#include "util/particle.h"

#define STDC17 201710L
#define TSTEP ( (double) ( __OBDS_TIME_STEP__ ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_PARTICLES__ ) )
#define LENGTH ( (double) ( __OBDS_LENGTH__ ) )
#define ISOTROPIC_RESISTANCE ( FCONF_ISOTROPIC_HYDRODYNAMIC_RESISTANCE_PARTICLE )
#define ISOTROPIC ( (bool) ( __OBDS_ISOTROPIC_HYDRODYNAMIC_RESISTANCE__ ) )
#define ANISOTROPIC ( (bool) ( !(ISOTROPIC) ) )

// defines the default callback for computing the ``effective'' mobility of spheres
static void default_particle_mobility_callback (particle_t* particles)
{
#define LINEAR_DETERMINISTIC_MOBILITY ( (double) ( __OBDS_TIME_STEP__ ) )
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  constexpr double linear_mobility = LINEAR_DETERMINISTIC_MOBILITY;
#else
  double const linear_mobility = LINEAR_DETERMINISTIC_MOBILITY;
#endif
  double* mobilities = &(particles -> bitmask -> data);
  mobilities[0] = linear_mobility;
  mobilities[1] = linear_mobility;
}

#if (ISOTROPIC_RESISTANCE == 0x00000001)

// translates the particles by updating the respective component of the position vectors
static void translate (double* __restrict__ x,
		       const double* __restrict__ F_x,
		       double const linear_mobility)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += linear_mobility * F_x[i];
  }
}

// translates the particles in the x, y, and z directions
static void translate_isotropic (particle_t* particles, void (*cb)(particle_t* particles))
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
  void (*callback)(particle_t*) = cb;
  callback(particles);
  const double* mobilities = &(particles -> bitmask -> data);
  double const linear_mobility = mobilities[0];
  // updates the position vectors subjected to the periodicity of the system box
  translate(x, f_x, linear_mobility);
  translate(y, f_y, linear_mobility);
  translate(z, f_z, linear_mobility);
  // updates the ``absolute'' position vectors
  translate(r_x, f_x, linear_mobility);
  translate(r_y, f_y, linear_mobility);
  translate(r_z, f_z, linear_mobility);
}

// delegates the task of translating the isotropic resistance particles (or spheres)
static void util_particle_translate_base (particle_t* particles,
					  void (*callback)(particle_t*))
{
  translate_isotropic(particles, callback);
}

// variadic particle translation function, for the callback is an optional argument
void util_particle_translate_varg (particle_t* particles, struct mobility mobility)
{
  void (*default_cb) (particle_t*) = default_particle_mobility_callback;
  void (*callback) (particle_t*) = (mobility.callback)? mobility.callback : default_cb;
  util_particle_translate_base(particles, callback);
}

#else

// NOTE: not really implemented (we are treating the anisotropic particles as spheres)
static void translate (double* __restrict__ x,
		       const double* __restrict__ F_x,
		       double const linear_parallel_mobility,
		       double const linear_orthogonal_mobility)
{
  double const parallel = linear_parallel_mobility;
  double const orthogonal = linear_orthogonal_mobility;
  double const linear_mobility = ( parallel + (0 * orthogonal) );
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += linear_mobility * F_x[i];
  }
}

// translates the anisotropic resistance particles in the x, y, and z directions
static void translate_anisotropic (particle_t* particles,
				   void (*cb)(particle_t* particles))
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
  void (*callback)(particle_t*) = cb;
  callback(particles);
  const double* mobilities = &(particles -> bitmask -> data);
  double const linear_parallel_mobility = mobilities[0];
  double const linear_orthogonal_mobility = mobilities[1];
  // updates the position vectors subjected to the periodicity of the system box
  translate(x, f_x, linear_parallel_mobility, linear_orthogonal_mobility);
  translate(y, f_y, linear_parallel_mobility, linear_orthogonal_mobility);
  translate(z, f_z, linear_parallel_mobility, linear_orthogonal_mobility);
  // updates the ``absolute'' position vectors
  translate(r_x, f_x, linear_parallel_mobility, linear_orthogonal_mobility);
  translate(r_y, f_y, linear_parallel_mobility, linear_orthogonal_mobility);
  translate(r_z, f_z, linear_parallel_mobility, linear_orthogonal_mobility);
}

// delegates the task of translating the anisotropic resistance particles
static void util_particle_translate_base (particle_t* particles,
					  void (*callback)(particle_t*))
{
  // we shall remove this safe guard once we add minimal code for anisotropic particles
  _Static_assert(ANISOTROPIC, "unimplemented error");
  translate_anisotropic(particles, callback);
}

// variadic particle translation function, for the callback is an optional argument
void util_particle_translate_varg (particle_t* particles, struct mobility mobility)
{
  void (*default_cb) (particle_t*) = default_particle_mobility_callback;
  void (*callback)(particle_t*) = (mobility.callback)? mobility.callback : default_cb;
  util_particle_translate_base(particles, callback);
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

// forwards the task of applying boundary conditions
static void pbcs (particle_t* particles)
{
  system_box_apply_periodic_boundary_conditions(particles);
}

// user-interface to the method that applies periodic boundary conditions on the particles
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
[3] https://stackoverflow.com/questions/1472138/c-default-arguments/2926165#2926165

*/
