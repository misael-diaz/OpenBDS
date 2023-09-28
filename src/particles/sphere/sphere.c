#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <endian.h>
#include <errno.h>
#include <float.h>
#include <math.h>

#include "io/logger.h"
#include "bds/params.h"
#include "system/box.h"
#include "particles/sphere/params.h"
#include "particles/sphere/type.h"
#include "util/particle.h"
#include "util/arrays.h"
#include "util/type.h"

#define STDC17 201710L
#define SUCCESS ( (int) ( __OBDS_SUCCESS__ ) )
#define FAILURE ( (int) ( __OBDS_FAILURE__ ) )
#define LIMIT ( (double) ( __OBDS_LIMIT__ ) )
#define LENGTH ( (double) ( __OBDS_LENGTH__ ) )
#define SIZED ( (double) ( __OBDS_NUM_SPHERES__ ) )
#define TSTEP ( (double) ( __OBDS_TIME_STEP__ ) )
#define NUMEL ( (size_t) ( __OBDS_NUM_SPHERES__ ) )
#define LOG_NUMEL ( (size_t) ( __OBDS_LOG_NUM_SPHERES__ ) )
#define EPSILON ( (double) ( __OBDS_SPH_EPSILON__ ) )
#define RADIUS ( (double) ( __OBDS_SPH_RADIUS__ ) )
#define CONTACT ( (double) ( __OBDS_SPH_CONTACT__ ) )
#define LINEAR_BROWNIAN_MOBILITY ( (double) ( __OBDS_SPH_LIN_BROWNIAN_MOBILITY__ ) )
#define ANGULAR_BROWNIAN_MOBILITY ( (double) ( __OBDS_SPH_ANG_BROWNIAN_MOBILITY__ ) )
#define CONTACT2 ( (double) ( ( CONTACT ) * ( CONTACT ) ) )
#define RANGE ( (double) ( __OBDS_SPH_RANGE__ ) )
#define MANTISSA ( (uint64_t) 0x000fffffffffffff )
#define MSB ( (uint64_t) 0x8000000000000000 )
#define TWOS_COMPLEMENT(x) ( ( ~(x) ) + 1 )
#define NOT_ZERO(x) ( ( ( ( ( (x) & (~MSB) ) + (~MSB) ) & MSB ) >> 63 ) )
#define NOT_SELF(x) ( TWOS_COMPLEMENT( NOT_ZERO( (x) ) ) )
#define EXP(x) ( ( ( (x) >> 52) & 0x7ff ) )
#define CLAMP (0.0625 / TSTEP)

static void copy (const prop_t* __restrict__ src, prop_t* __restrict__ dst)
{
  util_array_copy(src, dst);
}

static void zeros (prop_t* x)
{
  util_array_zeros(x);
}

static void ones (prop_t* x)
{
  util_array_ones(x);
}

static void iota (prop_t* id)
{
  util_array_iota(id);
}

// static uint64_t nexp (uint64_t const x)
//
// Synopsis:
// Negative Exponent.
// Yields a 64-bit bitmask of `ones' if the floating point number whose binary floating
// point representation is `x' has a negative exponent (n < 0) meaning that it is less
// than one, otherwise it yields a bitmask of `zeros' because it is greater than or equal
// to one (n >= 0).


static uint64_t nexp (uint64_t const x)
{
  // if `hi' is one, then the exponent `n' satisfies n <= 0, otherwise n > 0
  uint64_t h = ( ( ( (EXP(x) & 0x400) >> 10 ) + 1 ) & 1 );
  // if `lo' is one, then the exponent `n' satisfies n < 0, otherwise n == 0
  uint64_t l = ( ( ( ( ( (EXP(x) & 0x3ff) ^ 0x3ff ) & 0x3ff ) + 0x3ff ) & 0x400 ) >> 10 );
  // (hi & lo) yields one if the exponent `n' satisfies n < 0, zero otherwise (n >= 0)
  return TWOS_COMPLEMENT(h & l);
}


// static void inrange(dist, bitmask)
//
// Synopsis:
// Yields a 64-bit bitmask of `ones' for interacting pairs of particles, `zeros' otherwise
// for non-interacting pairs.
//
// Notes:
// If the interpaticle distance `dist' satisfies dist < 1, the pair interacts, the pair
// does not interact otherwise (dist >= 1). The caller method scales the interparticle
// distance so that the interparticle distance `dist' is O(1).
//
// We use the NOT_SELF(x) MACRO to mask the ith particle itself, in this case the distance
// is zero (64-bits of zeros in binary according to IEEE 754). If we didn't we would have
// an infinite force acting on the ith particle itself. The method does not care (and does
// not need to care) about the location of ith particle in the array `dist'; instead, it
// just masks whatever array element has a zero value. Normally this is not a problem
// because the particles are initialized in a grid, the interaction forces are repulsive,
// and the default time-step is enough to capture the particle dynamics.
//
// Parameters:
// dist		(read-only) the interparticle distance relative to the interaction length
// bitmask	on entry contains whatever the method does not care, on exit we have a 64
// 		bitmask of `ones' for interacting pairs and a bitmask of `zeros' otherwise
// 		for non-interacting pairs


static void inrange (const prop_t* __restrict__ dist, prop_t* __restrict__ bitmask)
{
  uint64_t* b = &bitmask[0].bin;
  const uint64_t* r = &dist[0].bin;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    b[i] = ( NOT_SELF(r[i]) & nexp(r[i]) );
  }
}


// zeroes the x, y, and z components of the force
static void zeroes (prop_t* __restrict__ f_x,
		    prop_t* __restrict__ f_y,
		    prop_t* __restrict__ f_z)
{
  zeros(f_x);
  zeros(f_y);
  zeros(f_z);
}


// static double max (vectors)
//
// Synopsis:
// Yields the absolute maximum component of the OBDS property `vectors' (often the forces)
// We do not care about the direction (signness) of the components of the vectors, just
// their magnitudes. This is why the signbit (the Most Significant Bit MSB) is ignored.
// This method is useful to see how near is the initialization run (at the highest verbose
// level) from reaching steady-state.
//
// Notes:
// Since the x, y, and z components are contiguous in memory by design we can afford to
// pass a pointer to the `x' component array and traverse the other two arrays in one
// sweep since we know their sizes at compile-time.


static double max (const prop_t* vectors)
{
  double max = 0;
  for (size_t i = 0; i != (3 * NUMEL); ++i)
  {
    uint64_t const bin = vectors[i].bin;
    prop_t const p = { .bin = ( bin & (~MSB) ) };
    double const data = p.data;
    if (data > max)
    {
      max = data;
    }
  }
  return max;
}


// as max(), yields the absolute minimum component of the OBDS property `vector'
static double min (const prop_t* vectors)
{
  double min = DBL_MAX;
  for (size_t i = 0; i != (3 * NUMEL); ++i)
  {
    uint64_t const bin = vectors[i].bin;
    prop_t const p = { .bin = ( bin & (~MSB) ) };
    double const data = p.data;
    if (data < min)
    {
      min = data;
    }
  }
  return min;
}


// void SLJ (dist, force, bitmask)
//
// Synopsis:
// Computes the force-to-distance ratio, F / r, of the Shifted Lennard-Jones SLJ (like)
// interaction, where `F' is the force on the ith particle due to its interaction with the
// jth particles, and `r' is the respective distance.
//
// Parameters:
// dist		on entry the array of size NUMEL stores the distances of the jth particles
//              relative to the ith particle, on exit it holds whatever, for it is also
//              used to store intermediate computations (caller must be aware of this)
//
// force	on entry it holds whatever it happens be on memory, on exit it holds the
// 		force-to-distance ratio
//
// bitmask	both on entry and exit it holds whatever it happens to be in memory, the
//              caller method must be aware of this. The bitmask is used to mask particles
//              that do not interact with the ith particle. This method delegates the
//              computation of the bitmask to the inrange method().


static void SLJ (prop_t* __restrict__ dist,
		 prop_t* __restrict__ force,
		 prop_t* __restrict__  bitmask)
{

  // scales the interparticle distance (as required by the inrange() method):

  double* r = &dist[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    double const c = (1.0 / RANGE);
    r[j] *= c;
  }

  // generates bitmask to mask non-interacting particle pairs (zero force between them):

  inrange(dist, bitmask);

  // restores the original scale of the interparticle distance:

  for (size_t j = 0; j != NUMEL; ++j)
  {
    double const c = RANGE;
    r[j] *= c;
  }

  // computes the "point-force" contribution of the SLJ interaction:

  double* frc = &force[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    // radius of cutoff
    double const r_c = RANGE;
    double const r_c7 = (r_c * r_c * r_c * r_c * r_c * r_c * r_c);
    double const r7 = (r[j] * r[j] * r[j] * r[j] * r[j] * r[j] * r[j]);
    double const r8 = r[j] * r7;
    // short-ranged point-force
    frc[j] = ( 1.0 - (r7 / r_c7) ) * (1.0 / r8);
  }

  // stores the SLJ force-to-distance ratio in the temporary placeholder:

  for (size_t j = 0; j != NUMEL; ++j)
  {
    double const c2 = CONTACT2;
    double const eps = EPSILON;
    double const c6 = (c2 * c2 * c2);
    double const kappa = (4.0 * eps * c6);
    double const k = (6.0 * kappa);
    r[j] = k * frc[j];
  }

  // zeros the SLJ force-to-distance ratio for non-interacting (pairs of) particles:

  uint64_t* f = &force[0].bin;
  const uint64_t* d = &dist[0].bin;
  const uint64_t* b = &bitmask[0].bin;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    f[j] = (b[j] & d[j]);
  }
}


// void pairs(i, offset_x, offset_y, offset_z, x, y, z, F_x, F_y, F_z, f, d, bitmask)
//
// Synopsis:
// Computes the force on the ith particle exerted by the jth (interacting) particles.
//
// Notes:
// The offsets are used to account for the periodic boundaries of the system box. The
// caller method handles the setting of these parameters. This method updates the ith
// element of the force components F_x, F_y, and F_z.
//
// Parameters:
// i		(read-only) the positional index of the ith particle
// offset_x	(read-only) offset in the x dimension to be applied to the jth particles
// offset_y	(read-only) offset in the y dimension to be applied to the jth particles
// offset_z	(read-only) offset in the z dimension to be applied to the jth particles
// x		(read-only) array of size NUMEL storing the x components of the position
// 			    vectors of the particles
// y		(read-only) array of size NUMEL storing the y components of the position
// 			    vectors of the particles
// z		(read-only) array of size NUMEL storing the z components of the position
// 			    vectors of the particles
// F_x		array of size NUMEL storing the x components of the force
// F_y		array of size NUMEL storing the y components of the force
// F_z		array of size NUMEL storing the z components of the force
// f		array of size NUMEL used for storing intermediate results
// d		array of size NUMEL used for storing intermediate results
// bitmask	array of size NUMEL used for storing intermediate results


static void pairs (size_t const i,
		   double const offset_x,
		   double const offset_y,
		   double const offset_z,
		   const prop_t* __restrict__ p_x,
		   const prop_t* __restrict__ p_y,
		   const prop_t* __restrict__ p_z,
		   prop_t* __restrict__ p_F_x,
		   prop_t* __restrict__ p_F_y,
		   prop_t* __restrict__ p_F_z,
		   prop_t* __restrict__ p_f,
		   prop_t* __restrict__ p_d,
		   prop_t* __restrict__ p_bitmask)
{
  // computes the interparticle distance between the ith and jth particles:

  double* d = &p_d[0].data;
  const double* x = &p_x[0].data;
  const double* y = &p_y[0].data;
  const double* z = &p_z[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    double const d_x = ( x[i] - (x[j] + offset_x) );
    double const d_y = ( y[i] - (y[j] + offset_y) );
    double const d_z = ( z[i] - (z[j] + offset_z) );
    double const d2 = d_x * d_x + d_y * d_y + d_z * d_z;
    d[j] = d2;
  }

  for (size_t j = 0; j != NUMEL; ++j)
  {
    d[j] = sqrt(d[j]);
  }

  // obtains the force-to-distance ratio, F / r:

  SLJ(p_d, p_f, p_bitmask);

  // updates the x, y, and z components of the force on the ith particle:

  for (size_t j = 0; j != NUMEL; ++j)
  {
    d[j] = ( x[i] - (x[j] + offset_x) );
  }

  const double* f = &p_f[0].data;
  double* force = &p_bitmask[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    force[j] = f[j] * d[j];
  }

  double* F_x = &p_F_x[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    F_x[i] += force[j];
  }

  for (size_t j = 0; j != NUMEL; ++j)
  {
    d[j] = ( y[i] - (y[j] + offset_y) );
  }

  for (size_t j = 0; j != NUMEL; ++j)
  {
    force[j] = f[j] * d[j];
  }

  double* F_y = &p_F_y[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    F_y[i] += force[j];
  }

  for (size_t j = 0; j != NUMEL; ++j)
  {
    d[j] = ( z[i] - (z[j] + offset_z) );
  }

  for (size_t j = 0; j != NUMEL; ++j)
  {
    force[j] = f[j] * d[j];
  }

  double* F_z = &p_F_z[0].data;
  for (size_t j = 0; j != NUMEL; ++j)
  {
    F_z[i] += force[j];
  }
}


// defines the callback required by the method that computes the interparticle forces
static void callback (particle_t* particles,
		      size_t const i,
		      double const offset_x,
		      double const offset_y,
		      double const offset_z)
{
  const prop_t* x = particles -> x;
  const prop_t* y = particles -> y;
  const prop_t* z = particles -> z;
  prop_t* f_x = particles -> f_x;
  prop_t* f_y = particles -> f_y;
  prop_t* f_z = particles -> f_z;
  prop_t* tmp = particles -> tmp;
  prop_t* temp = particles -> temp;
  prop_t* bitmask = particles -> bitmask;
  pairs(i, offset_x, offset_y, offset_z, x, y, z, f_x, f_y, f_z, tmp, temp, bitmask);
}


// clamps forces larger than CLAMP to CLAMP; in other words, this method makes sure that
// there is no force (component) larger than the value represented by the CLAMP MACRO.
static void clamp (prop_t* __restrict__ p_force,
		   prop_t* __restrict__ p_tmp,
		   prop_t* __restrict__ p_temp,
		   prop_t* __restrict__ p_bitmask)
{
  double* temp = &p_temp[0].data;
  double* force = &p_force[0].data;
  // stores the scaled force in the temporary (required for masking)
  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const c = (1.0 / CLAMP);
    temp[i] = c * force[i];
  }

  uint64_t* t = &p_temp[0].bin;
  uint64_t* b = &p_bitmask[0].bin;
  // masks the particles that do not require clamping
  for (size_t i = 0; i != NUMEL; ++i)
  {
    b[i] = nexp(t[i]);
  }

  uint64_t* f = &p_force[0].bin;
  // stores the forces that do not require clamping
  for (size_t i = 0; i != NUMEL; ++i)
  {
    t[i] = (f[i] & b[i]);
  }

  uint64_t* a = &p_tmp[0].bin;
  prop_t const clamp = { .data = CLAMP };
  uint64_t const max = clamp.bin;
  // clamps with the maximum force in the same sense as the (original) force
  for (size_t i = 0; i != NUMEL; ++i)
  {
    a[i] = ( ( f[i] & MSB ) | ( max & (~b[i]) ) );
  }

  double* tmp = &p_tmp[0].data;
  // addes the stored forces to finalize the clamping
  for (size_t i = 0; i != NUMEL; ++i)
  {
    force[i] = (tmp[i] + temp[i]);
  }
}


// clamps the x, y, and z components of the force
static void clamps (prop_t* __restrict__ f_x,
		    prop_t* __restrict__ f_y,
		    prop_t* __restrict__ f_z,
		    prop_t* __restrict__ tmp,
		    prop_t* __restrict__ temp,
		    prop_t* __restrict__ bitmask)
{
  clamp(f_x, tmp, temp, bitmask);
  clamp(f_y, tmp, temp, bitmask);
  clamp(f_z, tmp, temp, bitmask);
}


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


static int stochastic_force (random_t* random, prop_t* p_F_x)
{
  double* F_x = &p_F_x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    double const rand = random -> fetch(random);
    if (status(rand) == FAILURE)
    {
      return FAILURE;
    }
    F_x[i] = rand;
  }

  return SUCCESS;
}


static int stochastic_forces (random_t* random, prop_t* f_x, prop_t* f_y, prop_t* f_z)
{
  if (stochastic_force(random, f_x) == FAILURE)
  {
    return FAILURE;
  }

  if (stochastic_force(random, f_y) == FAILURE)
  {
    return FAILURE;
  }

  if (stochastic_force(random, f_z) == FAILURE)
  {
    return FAILURE;
  }

  return SUCCESS;
}


// shifts the particles along the x, y, or z axis due to deterministic force effects
static void shift (prop_t* __restrict__ prop_x, const prop_t* __restrict__ prop_F_x)
{
  double const dt = TSTEP;
  double* x = &prop_x[0].data;
  const double* F_x = &prop_F_x[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += (dt * F_x[i]);
  }
}


static void stochastic_shift (prop_t* __restrict__ prop_x,
			      const prop_t* __restrict__ prop_F_x)
{
  double* x = &prop_x[0].data;
  const double* F_x = &prop_F_x[0].data;
  double const linear_stochastic_mobility = LINEAR_BROWNIAN_MOBILITY;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += (linear_stochastic_mobility * F_x[i]);
  }
}


// stores the change in the Euler angle `x'
static void stochastic_rotation (prop_t* __restrict__ prop_x,
				 const prop_t* __restrict__ prop_T_x)
{
  double* x = &prop_x[0].data;
  const double* T_x = &prop_T_x[0].data;
  double const angular_stochastic_mobility = ANGULAR_BROWNIAN_MOBILITY;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = (angular_stochastic_mobility * T_x[i]);
  }
}


// shifts the particles along the axes owing to the net deterministic forces
static void shifts (prop_t* __restrict__ x,
		    prop_t* __restrict__ y,
		    prop_t* __restrict__ z,
		    const prop_t* __restrict__ f_x,
		    const prop_t* __restrict__ f_y,
		    const prop_t* __restrict__ f_z)
{
  shift(x, f_x);
  shift(y, f_y);
  shift(z, f_z);
}


static void stochastic_shifts (prop_t* __restrict__ x,
			       prop_t* __restrict__ y,
			       prop_t* __restrict__ z,
			       const prop_t* __restrict__ f_x,
			       const prop_t* __restrict__ f_y,
			       const prop_t* __restrict__ f_z)
{
  stochastic_shift(x, f_x);
  stochastic_shift(y, f_y);
  stochastic_shift(z, f_z);
}


// kinematics (x, y, z, _dx, _dy, _dz, d_x, d_y, d_z)
//
// Synopsis:
// Obtains the new director (or the orientation vector) via the kinematic equation.
//
// Parameters:
// x, y, z		components of the new director (at time t + dt)
// _dx, _dy, _dz	components of the angular displacement vector
// d_x, d_y, d_z	components of the current director (at time t)


static void kinematics (prop_t* __restrict__ p_x,
			prop_t* __restrict__ p_y,
			prop_t* __restrict__ p_z,
			const prop_t* __restrict__ p_dx,
			const prop_t* __restrict__ p_dy,
			const prop_t* __restrict__ p_dz,
			const prop_t* __restrict__ p_d_x,
			const prop_t* __restrict__ p_d_y,
			const prop_t* __restrict__ p_d_z)

{
  double* x = &p_x[0].data;
  double* y = &p_y[0].data;
  double* z = &p_z[0].data;
  const double* _dx = &p_dx[0].data;
  const double* _dy = &p_dy[0].data;
  const double* _dz = &p_dz[0].data;
  const double* d_x = &p_d_x[0].data;
  const double* d_y = &p_d_y[0].data;
  const double* d_z = &p_d_z[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] = d_x[i] + (_dy[i] * d_z[i] - _dz[i] * d_y[i]);
    y[i] = d_y[i] + (_dz[i] * d_x[i] - _dx[i] * d_z[i]);
    z[i] = d_z[i] + (_dx[i] * d_y[i] - _dy[i] * d_x[i]);
  }
}


// normalizes the vector (makes unit vector) whose components are x, y, and z
static void normalize (prop_t* __restrict__ p_x,
		       prop_t* __restrict__ p_y,
		       prop_t* __restrict__ p_z,
		       prop_t* __restrict__ p_v,
		       prop_t* __restrict__ p_t)
{
  double* x = &p_x[0].data;
  double* y = &p_y[0].data;
  double* z = &p_z[0].data;
  double* v = &p_v[0].data;
  double* t = &p_t[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    v[i] = (x[i] * x[i]) + (y[i] * y[i]) + (z[i] * z[i]);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    t[i] = sqrt(v[i]);
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    v[i] = 1.0 / t[i];
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] *= v[i];
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    y[i] *= v[i];
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    z[i] *= v[i];
  }
}


// updates the Euler angle `x' with the angular displacement stored in `_dx'
static void update_Euler_angle (prop_t* __restrict__ prop_x,
				const prop_t* __restrict__ prop_dx)
{
  double* x = &prop_x[0].data;
  const double* _dx = &prop_dx[0].data;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] += _dx[i];
  }
}


// void stochastic_rotations (a_x, a_y, a_z,
// 			      d_x, d_y, d_z,
// 			        x,   y,   z,
// 			      _dx, _dy, _dz, tmp,
// 			      t_x, t_y, t_z)
//
// Synopsis:
// Updates the Euler angles and the director (or orientation vector).
//
// Parameters:
// a_x, a_y, a_z	components of the Euler angles
// d_x, d_y, d_z	components of the director
// x, y, z		placeholders for storing the new director
// _dx, _dy, _dz	placeholders for storing the change in the Euler angles
// tmp			general purpose placeholder
// t_x, t_y, t_z	components of the torque


static void stochastic_rotations (prop_t* __restrict__ a_x,
				  prop_t* __restrict__ a_y,
				  prop_t* __restrict__ a_z,
				  prop_t* __restrict__ d_x,
				  prop_t* __restrict__ d_y,
				  prop_t* __restrict__ d_z,
				  prop_t* __restrict__ x,
				  prop_t* __restrict__ y,
				  prop_t* __restrict__ z,
				  prop_t* __restrict__ _dx,
				  prop_t* __restrict__ _dy,
				  prop_t* __restrict__ _dz,
				  prop_t* __restrict__ tmp,
				  const prop_t* __restrict__ t_x,
				  const prop_t* __restrict__ t_y,
				  const prop_t* __restrict__ t_z)
{
  // updates the Euler angles:

  stochastic_rotation(_dx, t_x);
  update_Euler_angle(a_x, _dx);
  stochastic_rotation(_dy, t_y);
  update_Euler_angle(a_y, _dy);
  stochastic_rotation(_dz, t_z);
  update_Euler_angle(a_z, _dz);

  // updates the director:

  kinematics(x, y, z, _dx, _dy, _dz, d_x, d_y, d_z);

  // normalizes the director:

  prop_t* v = _dx;
  normalize(x, y, z, v, tmp);
  copy(x, d_x);
  copy(y, d_y);
  copy(z, d_z);
}


// places the spheres in a grid (or lattice) like structure for system initialization
static void grid (prop_t* __restrict__ xprop,
		  prop_t* __restrict__ yprop,
		  prop_t* __restrict__ zprop)
{
  // gets the number of spheres (at contact) that can be fitted along any dimension:

  size_t const count = (LIMIT / RADIUS);
  size_t const count2 = (count * count);

  // sets particles at grid locations:

  double* x = &xprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n % count);
    double const pos = RADIUS + CONTACT * ( (double) i );
    x[n] = pos;
  }

  double* y = &yprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n % count2) / count;
    double const pos = RADIUS + CONTACT * ( (double) i );
    y[n] = pos;
  }

  double* z = &zprop[0].data;
  for (size_t n = 0; n != NUMEL; ++n)
  {
    size_t const i = (n / count2);
    double const pos = RADIUS + CONTACT * ( (double) i );
    z[n] = pos;
  }

  // shifts the particles so that their coordinates are in the range [-LIMIT, +LIMIT]:

  for (size_t i = 0; i != NUMEL; ++i)
  {
    x[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    y[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUMEL; ++i)
  {
    z[i] -= LIMIT;
  }
}


// sums `src' and `dst' vectors (elementwise), stores the result in `dst'
static void vsum (prop_t* __restrict__ dest, const prop_t* __restrict__ source)
{
  double* dst = &dest[0].data;
  const double* src = &source[0].data;
  for (size_t i = 0; i != (3 * NUMEL); ++i)
  {
    dst[i] += src[i];
  }
}


// updates the positions of the particles due to the forces acting on them
static int updater (sphere_t* spheres)
{
  prop_t* x = spheres -> props -> x;
  prop_t* y = spheres -> props -> y;
  prop_t* z = spheres -> props -> z;
  prop_t* r_x = spheres -> props -> r_x;
  prop_t* r_y = spheres -> props -> r_y;
  prop_t* r_z = spheres -> props -> r_z;
  prop_t* a_x = spheres -> props -> a_x;
  prop_t* a_y = spheres -> props -> a_y;
  prop_t* a_z = spheres -> props -> a_z;
  prop_t* d_x = spheres -> props -> d_x;
  prop_t* d_y = spheres -> props -> d_y;
  prop_t* d_z = spheres -> props -> d_z;
  prop_t* f_x = spheres -> props -> f_x;
  prop_t* f_y = spheres -> props -> f_y;
  prop_t* f_z = spheres -> props -> f_z;
  prop_t* t_x = spheres -> props -> t_x;
  prop_t* t_y = spheres -> props -> t_y;
  prop_t* t_z = spheres -> props -> t_z;
  // uses these properties as temporary placeholders
  prop_t* tmp = spheres -> props -> tmp;
  prop_t* temp = spheres -> props -> temp;
  prop_t* bitmask = spheres -> props -> bitmask;
  prop_t* list = spheres -> props -> list;
  random_t* random = spheres -> prng;
  zeroes(f_x, f_y, f_z);
  void (*cb) (particle_t* particles,
	      size_t const i,
	      double const offset_x,
	      double const offset_y,
	      double const offset_z) = callback;
  particle_t* particles = spheres -> props;
  util_particle_brute_force(particles, cb);
  clamps(f_x, f_y, f_z, tmp, temp, bitmask);
  shifts(r_x, r_y, r_z, f_x, f_y, f_z);
  shifts(x, y, z, f_x, f_y, f_z);
  // we want to store the deterministic forces temporarily for logging purposes
  memcpy(list, f_x, 3LU * NUMEL * sizeof(prop_t));
  if (stochastic_forces(random, f_x, f_y, f_z) == FAILURE)
  {
    return FAILURE;
  }
  stochastic_shifts(r_x, r_y, r_z, f_x, f_y, f_z);
  stochastic_shifts(x, y, z, f_x, f_y, f_z);
  vsum(f_x, list);

  if (stochastic_forces(random, t_x, t_y, t_z) == FAILURE)
  {
    return FAILURE;
  }

  prop_t* _x = tmp;
  prop_t* _y = temp;
  prop_t* _z = bitmask;
  prop_t* iter = list;
  prop_t* _dx = iter;
  iter += NUMEL;
  prop_t* _dy = iter;
  iter += NUMEL;
  prop_t* _dz = iter;
  iter += NUMEL;
  prop_t* t = iter;
  iter += NUMEL;
  stochastic_rotations(a_x, a_y, a_z,
		       d_x, d_y, d_z,
		        _x,  _y,  _z,
		       _dx, _dy, _dz, t,
		       t_x, t_y, t_z);

  util_particle_pbcs(particles);
  return SUCCESS;
}


// dumps BDS parameters to a plain txt file (usually for and post-proccesing)
static int info ()
{
  const char fname[] = "run/bds/data/params/params-bds.txt";
  FILE* file = fopen(fname, "w");
  if (file == NULL)
  {
    const char errmsg[] = "info(): IO ERROR with file %s: %s\n";
    fprintf(stderr, errmsg, fname, strerror(errno));
    return FAILURE;
  }

  fprintf(file, "LIMIT:       %.16e\n", LIMIT);
  fprintf(file, "LENGTH:      %.16e\n", LENGTH);
  fprintf(file, "TIME_STEP:   %.16e\n", TSTEP);
  fprintf(file, "NUM_SPHERES: %lu\n",   NUMEL);

  fclose(file);
  return SUCCESS;
}


// logs some properties of interest such as the positions of the particles
static int logger (const sphere_t* spheres, size_t const step)
{
  char log[80];
  sprintf(log, "run/bds/data/positions/spheres-%zu", step);
  const particle_t* particles = spheres -> props;
  return io_logger_log(particles, log);
}


// default logger, just logs the properties of interest to a plain text file
static int logger_default (const sphere_t* spheres, size_t const step)
{
  if (logger(spheres, step) == FAILURE)
  {
    fprintf(stderr, "logger(): ERROR\n");
    return FAILURE;
  }

  return SUCCESS;
}


// as the default logger but also logs some info on the console
static int logger_verbose (const sphere_t* spheres, size_t const step)
{
  if (logger(spheres, step) == FAILURE)
  {
    fprintf(stderr, "logger(): ERROR\n");
    return FAILURE;
  }

  const prop_t* force = spheres -> props -> f_x;
  double const f_min = min(force);
  double const f_max = max(force);
  if (f_max == CLAMP)
  {
    fprintf(stdout, "logger(): clamping detected in step %zu\n", step);
  }

  fprintf(stdout, "logger(): min: %.16e max: %.16e\n", f_min, f_max);

  return SUCCESS;
}


// initializes the spheres object from the available memory in the workspace
sphere_t* particles_sphere_initializer (void* workspace, SPHLOG LVL)
{
  // compile-time sane checks:

#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
  static_assert(BYTE_ORDER == LITTLE_ENDIAN);
#if defined(FLOAT_WORD_ORDER)
  static_assert(FLOAT_WORD_ORDER == LITTLE_ENDIAN);
#endif
  static_assert( __OBDS_LOG_NUM_SPHERES__ >= 8LU );
  static_assert(sizeof(NUMEL) == 8);
  static_assert(sizeof(RADIUS) == 8);
  static_assert(sizeof(CONTACT) == 8);
  static_assert(sizeof(OBDS_Sphere_t) == 256);
  static_assert(sizeof(size_t) == sizeof(uint64_t));
  static_assert(sizeof(sphere_t) == 32);
  static_assert(sizeof(prop_t) == 8);
  static_assert(sizeof(random_t) == 16);
  static_assert(sizeof(generator_t) == 32);
  static_assert(sizeof(uint64_t) == 8);
  static_assert(sizeof(double) == 8);
  static_assert(NUMEL != 0);
  static_assert(NUMEL % 2 == 0);
  static_assert(SIZE_MAX == UINT64_MAX);
  static_assert(NUMEL <= 0x7ffffffffffffffe);
  static_assert( SIZED * (RADIUS * RADIUS * RADIUS) < (LIMIT * LIMIT * LIMIT) );
  static_assert(CONTACT == 2.0);
  static_assert(RADIUS == 1.0);
#else
  _Static_assert(BYTE_ORDER == LITTLE_ENDIAN, "expects little-endian byte-order");
#if defined(FLOAT_WORD_ORDER)
  _Static_assert(FLOAT_WORD_ORDER == LITTLE_ENDIAN, "expects little-endian byte-order");
#endif
  _Static_assert( __OBDS_LOG_NUM_SPHERES__ >= 8LU, "expects log2(NUM_SPHERES) >= 8" );
  _Static_assert(sizeof(NUMEL) == 8, "expects 8 bytes");
  _Static_assert(sizeof(RADIUS) == 8, "expects 8 bytes");
  _Static_assert(sizeof(CONTACT) == 8, "expects 8 bytes");
  _Static_assert(sizeof(OBDS_Sphere_t) == 256, "expects 256 bytes");
  _Static_assert(sizeof(size_t) == sizeof(uint64_t), "expects equal size");
  _Static_assert(sizeof(sphere_t) == 32, "expects 32 bytes");
  _Static_assert(sizeof(prop_t) == 8, "expects 8 bytes");
  _Static_assert(sizeof(random_t) == 16, "expects 16 bytes");
  _Static_assert(sizeof(generator_t) == 32, "expects 32 bytes");
  _Static_assert(sizeof(uint64_t) == 8, "expects 8 bytes");
  _Static_assert(sizeof(double) == 8, "expects 8 bytes");
  _Static_assert(NUMEL != 0, "expects non-zero value for NUM_SPHERES");
  _Static_assert(NUMEL % 2 == 0, "expects NUM_SPHERES to be even");
  _Static_assert(SIZE_MAX == UINT64_MAX, "expects equal range");
  _Static_assert(NUMEL <= 0x7fffffffffffffff, "expects fewer NUM_SPHERES");
#endif

  // runtime sane checks:

  if (RADIUS != 1.0)
  {
    errno = EINVAL;
    char err[] = "sphere-initializer() expects unit spheres: %s\n";
    fprintf(stderr, err, strerror(errno));
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }

  if (CONTACT != 2.0)
  {
    errno = EINVAL;
    char err[] = "sphere-initializer() expects unit spheres: %s\n";
    fprintf(stderr, err, strerror(errno));
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }

  if ( SIZED * (RADIUS * RADIUS * RADIUS) >= (LIMIT * LIMIT * LIMIT) )
  {
    errno = EINVAL;
    char err[] = "sphere-initializer() expects smaller volume fractions: %s\n";
    fprintf(stderr, err, strerror(errno));
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }

  prop_t const dt = { .data = TSTEP };
  uint64_t const bin = dt.bin;
  uint64_t const mantissa = (bin & MANTISSA);
  if (mantissa != 0)
  {
    errno = EINVAL;
    char err[] = "sphere-initializer() expects the time-step to be a power of two: %s\n";
    fprintf(stderr, err, strerror(errno));
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }


  // attemps to dump BDS parameters to a plain text file as a runtime check
  if (info() == FAILURE)
  {
    fprintf(stderr, "sphere-initializer(): ERROR\n");
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }


  // bindinds:

  void* iter = workspace;
  sphere_t* spheres = (sphere_t*) iter;
  iter += sizeof(sphere_t);

  spheres -> props = (OBDS_Sphere_t*) iter;
  iter += sizeof(OBDS_Sphere_t);

  spheres -> props -> x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> r_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> r_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> r_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> _dx = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> _dy = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> _dz = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> a_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> a_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> a_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> d_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> d_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> d_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> f_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> f_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> f_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> t_x = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> t_y = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> t_z = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> tmp = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> temp = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);
  spheres -> props -> bitmask = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> props -> list = (prop_t*) iter;
  iter += (NUMEL * LOG_NUMEL) * sizeof(prop_t);

  spheres -> props -> id = (prop_t*) iter;
  iter += NUMEL * sizeof(prop_t);

  spheres -> prng = (random_t*) iter;
  iter += sizeof(random_t);
  spheres -> prng -> generator = (generator_t*) iter;
  iter += sizeof(generator_t);
  spheres -> prng -> generator -> count = (double*) iter;
  iter += sizeof(double);
  spheres -> prng -> generator -> state = (uint64_t*) iter;
  iter += sizeof(uint64_t);

  spheres -> update = updater;

  switch (LVL)
  {
    case (SPH_LOG_LEVEL_VERBOSE):
	spheres -> log = logger_verbose;
	break;
    default:
      spheres -> log = logger_default;
  }

  prop_t* x = spheres -> props -> x;
  prop_t* y = spheres -> props -> y;
  prop_t* z = spheres -> props -> z;
  prop_t* r_x = spheres -> props -> r_x;
  prop_t* r_y = spheres -> props -> r_y;
  prop_t* r_z = spheres -> props -> r_z;
  prop_t* _dx = spheres -> props -> _dx;
  prop_t* _dy = spheres -> props -> _dy;
  prop_t* _dz = spheres -> props -> _dz;
  prop_t* a_x = spheres -> props -> a_x;
  prop_t* a_y = spheres -> props -> a_y;
  prop_t* a_z = spheres -> props -> a_z;
  prop_t* d_x = spheres -> props -> d_x;
  prop_t* d_y = spheres -> props -> d_y;
  prop_t* d_z = spheres -> props -> d_z;
  prop_t* f_x = spheres -> props -> f_x;
  prop_t* f_y = spheres -> props -> f_y;
  prop_t* f_z = spheres -> props -> f_z;
  prop_t* t_x = spheres -> props -> t_x;
  prop_t* t_y = spheres -> props -> t_y;
  prop_t* t_z = spheres -> props -> t_z;
  prop_t* tmp = spheres -> props -> tmp;
  prop_t* temp = spheres -> props -> temp;
  prop_t* bitmask = spheres -> props -> bitmask;
  prop_t* list = spheres -> props -> list;
  prop_t* id = spheres -> props -> id;

  // initializations:

  zeros(x);
  zeros(y);
  zeros(z);
  zeros(r_x);
  zeros(r_y);
  zeros(r_z);
  zeros(_dx);
  zeros(_dy);
  zeros(_dz);
  zeros(a_x);
  zeros(a_y);
  zeros(a_z);
  zeros(d_x);
  zeros(d_y);
  zeros(d_z);
  zeros(f_x);
  zeros(f_y);
  zeros(f_z);
  zeros(t_x);
  zeros(t_y);
  zeros(t_z);
  zeros(tmp);
  zeros(temp);
  zeros(bitmask);
  iota(list);
  iota(id);
  ones(d_z);

  grid(x, y, z);

  extern int util_random_initializer(random_t*, enum PRNG);
  iPRNG_t const irandom = { .initializer = util_random_initializer };
  util_t const util = { .random = irandom };
  if (util.random.initializer(spheres -> prng, NRAND) == FAILURE)
  {
    fprintf(stderr, "particles.sphere.initializer(): PRNG ERROR\n");
#if ( ( __GNUC__ > 12 ) && ( __STDC_VERSION__ > STDC17 ) )
    return nullptr;
#else
    return NULL;
#endif
  }

  return spheres;
}


/*

OpenBDS							September 06, 2023

source: particles/sphere/sphere.c
author: @misael-diaz

Synopsis:
Implements methods for spheres.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.
[3] https://en.cppreference.com
[4] https://www.man7.org/linux/man-pages/index.html
[5] https://www.gnu.org/software/libc/manual/html_node/index.html#SEC_Contents

*/
