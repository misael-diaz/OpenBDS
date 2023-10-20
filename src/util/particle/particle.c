#include "util/random/err.h"
#include "util/random/type.h"
#include "system/box/params.h"
#include "system/box/utils.h"
#include "system/params.h"
#include "particle/params.h"
#include "util/particle.h"
#include "bds/types/force.h"
#include "bds/params.h"

#define STDC17 201710L
#define SUCCESS ( (int) ( __OBDS_SUCCESS__ ) )
#define FAILURE ( (int) ( __OBDS_FAILURE__ ) )
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
#define ANGULAR_DETERMINISTIC_MOBILITY ( 0.75 * ( (double) ( __OBDS_TIME_STEP__ ) ) )
#define LINEAR_BROWNIAN_MOBILITY (\
	(double) ( __OBDS_ISOTROPIC_LINEAR_BROWNIAN_MOBILITY__ )\
)
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  constexpr double linear_mobility = LINEAR_DETERMINISTIC_MOBILITY;
  constexpr double angular_mobility = ANGULAR_DETERMINISTIC_MOBILITY;
  constexpr double linear_Brownian_mobility = LINEAR_BROWNIAN_MOBILITY;
//constexpr double angular_Brownian_mobility = ANGULAR_BROWNIAN_MOBILITY;
#else
  double const linear_mobility = LINEAR_DETERMINISTIC_MOBILITY;
  double const angular_mobility = ANGULAR_DETERMINISTIC_MOBILITY;
  double const linear_Brownian_mobility = LINEAR_BROWNIAN_MOBILITY;
//double const angular_Brownian_mobility = ANGULAR_BROWNIAN_MOBILITY;
#endif
  prop_t* placeholder = particles -> bitmask;
  double* mobilities = &(placeholder[0].data);
  mobilities[0] = linear_mobility;
  mobilities[1] = angular_mobility;
  mobilities[2] = linear_Brownian_mobility;
//mobilities[3] = angular_Brownian_mobility;
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
static void translate_isotropic (particle_t* particles, double const linear_mobility)
{
  // destructures the particles object to obtain the properties
  prop_t* prop_x = particles -> x;
  prop_t* prop_y = particles -> y;
  prop_t* prop_z = particles -> z;
  prop_t* prop_r_x = particles -> r_x;
  prop_t* prop_r_y = particles -> r_y;
  prop_t* prop_r_z = particles -> r_z;
  const prop_t* prop_f_x = particles -> f_x;
  const prop_t* prop_f_y = particles -> f_y;
  const prop_t* prop_f_z = particles -> f_z;
  // destructures the particle properties to obtain the underlying data
  double* x = &(prop_x[0].data);
  double* y = &(prop_y[0].data);
  double* z = &(prop_z[0].data);
  double* r_x = &(prop_r_x[0].data);
  double* r_y = &(prop_r_y[0].data);
  double* r_z = &(prop_r_z[0].data);
  const double* f_x = &(prop_f_x[0].data);
  const double* f_y = &(prop_f_y[0].data);
  const double* f_z = &(prop_f_z[0].data);
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
static void translate_base (particle_t* particles,
			    enum FORCE FORCE,
			    void (*callback)(particle_t*))
{
  // writes the particle ``mobilities'' to the designated placeholder
  callback(particles);
  // unpacks the linear ``mobilities'' deterministic (default) or Brownian
  const prop_t* placeholder = particles -> bitmask;
  const double* mobilities = &(placeholder[0].data);
  double const default_mobility = mobilities[0];
  double const Brownian_mobility = mobilities[2];
  double const mobility = ( (FORCE == BROWNIAN)? Brownian_mobility : default_mobility );
  translate_isotropic(particles, mobility);
}

// variadic particle translation function, for the callback is an optional argument
void util_particle_translate_varg (particle_t* particles,
				   enum FORCE FORCE,
				   struct mobility mobility)
{
  void (*default_cb) (particle_t*) = default_particle_mobility_callback;
  void (*callback) (particle_t*) = (mobility.callback)? mobility.callback : default_cb;
  translate_base(particles, FORCE, callback);
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
  // destructures the particles object to obtain the properties
  prop_t* prop_x = particles -> x;
  prop_t* prop_y = particles -> y;
  prop_t* prop_z = particles -> z;
  prop_t* prop_r_x = particles -> r_x;
  prop_t* prop_r_y = particles -> r_y;
  prop_t* prop_r_z = particles -> r_z;
  const prop_t* prop_f_x = particles -> f_x;
  const prop_t* prop_f_y = particles -> f_y;
  const prop_t* prop_f_z = particles -> f_z;
  // destructures the particle properties to obtain the underlying data
  double* x = &(prop_x[0].data);
  double* y = &(prop_y[0].data);
  double* z = &(prop_z[0].data);
  double* r_x = &(prop_r_x[0].data);
  double* r_y = &(prop_r_y[0].data);
  double* r_z = &(prop_r_z[0].data);
  const double* f_x = &(prop_f_x[0].data);
  const double* f_y = &(prop_f_y[0].data);
  const double* f_z = &(prop_f_z[0].data);
  // writes the particle ``mobilities'' to the designated placeholder
  void (*callback)(particle_t*) = cb;
  callback(particles);
  // gets the particle translational mobility when subjected to deterministic forces
  const prop_t* placeholder = particles -> bitmask;
  const double* mobilities = &(placeholder[0].data);
  // gets the particle translational mobility when subjected to deterministic forces
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
static void translate_base (particle_t* particles,
			    enum FORCE FORCE,
			    void (*callback)(particle_t*))
{
  // we shall remove this safe guard once we add minimal code for anisotropic particles
  _Static_assert(ANISOTROPIC, "unimplemented error");
  translate_anisotropic(particles, callback);
}

// variadic particle translation function, for the callback is an optional argument
void util_particle_translate_varg (particle_t* particles,
				   enum FORCE FORCE,
				   struct mobility mobility)
{
  void (*default_cb) (particle_t*) = default_particle_mobility_callback;
  void (*callback)(particle_t*) = (mobility.callback)? mobility.callback : default_cb;
  translate_base(particles, FORCE, callback);
}

#endif


// checks the underlying status code in the binary pattern of the floating-point number
// `status' to determine if an error related to the generation of pseudo-random numbers
// has occurred
static int status (double const status)
{
  prop_t const error = { .data = status };
  uint64_t const err = error.bin;
  if (err == OBDS_ERR_PRNG)
  {
    return FAILURE;
  }

  return SUCCESS;
}


// computes the `x' component of the stochastic (or random) force vector
static int BrownianForce (random_t* random, double* f_x)
{
  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const rand = random -> fetch(random);
    if (status(rand) == FAILURE)
    {
      return FAILURE;
    }
    f_x[i] = rand;
  }

  return SUCCESS;
}


// computes the `x' component of the stochastic (or random) torque vector
static int BrownianTorque (random_t* random, double* t_x)
{
  // we can afford to use the same function because both the stochastic forces and torques
  // are non-dimensional but share the same statistical properties of zero mean and unit
  // variance
  return BrownianForce(random, t_x);
}


// updates the components of the stochastic force vector
static int BrownianForces (random_t* random, particle_t* particles)
{
  double* f_x = &(particles -> f_x -> data);
  double* f_y = &(particles -> f_y -> data);
  double* f_z = &(particles -> f_z -> data);
  if (BrownianForce(random, f_x) == FAILURE)
  {
    return FAILURE;
  }

  if (BrownianForce(random, f_y) == FAILURE)
  {
    return FAILURE;
  }

  if (BrownianForce(random, f_z) == FAILURE)
  {
    return FAILURE;
  }

  return SUCCESS;
}


// updates the components of the stochastic torque vector
static int BrownianTorques (random_t* random, particle_t* particles)
{
  double* t_x = &(particles -> t_x -> data);
  double* t_y = &(particles -> t_y -> data);
  double* t_z = &(particles -> t_z -> data);
  if (BrownianTorque(random, t_x) == FAILURE)
  {
    return FAILURE;
  }

  if (BrownianTorque(random, t_y) == FAILURE)
  {
    return FAILURE;
  }

  if (BrownianTorque(random, t_z) == FAILURE)
  {
    return FAILURE;
  }

  return SUCCESS;
}


// void bruteForce(particles, callback)
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


static void bruteForce (particle_t* particles,
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
  system_box_applyPeriodicBoundaryConditions(particles);
}


// user-interface to the method that applies periodic boundary conditions on the particles
void util_particle_pbcs (particle_t* particles)
{
  pbcs(particles);
}


// user-interface to the method that computes the Brownian forces
int util_particle_BrownianForces (random_t* random, particle_t* particles)
{
  return BrownianForces(random, particles);
}


// user-interface to the method that computes the Brownian torques
int util_particle_BrownianTorques (random_t* random, particle_t* particles)
{
  return BrownianTorques(random, particles);
}


// user-interface to the method that computes the resultant deterministic forces
void util_particle_bruteForce (particle_t* particles,
			       void (*callback) (particle_t* particles,
						 size_t const i,
						 double const offset_x,
						 double const offset_y,
						 double const offset_z))
{
  bruteForce(particles, callback);
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
