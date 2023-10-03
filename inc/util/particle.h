#ifndef GUARD_OPENBDS_UTIL_PARTICLE_H
#define GUARD_OPENBDS_UTIL_PARTICLE_H

#include "bds/types.h"
#include "util/random/type.h"

// translates particles by the conservative forces acting on them, variadic, optional
// callback for spheres (all the other particle types must supply it)
#define util_particle_translate(particles, ...)\
	util_particle_translate_varg(particles, (struct mobility) { __VA_ARGS__ })

// callback for computing the particle mobility
struct mobility
{
  void (*callback) (particle_t*);
};

// forwards the task of translating the particles, defines optional mobility
void util_particle_translate_varg(particle_t*, struct mobility);
// applies periodic boundary conditions on the position vectors of the particles
void util_particle_pbcs(particle_t*);
// samples the stochastic forces from the Gausian PRNG
int util_particle_stochastic_forces(random_t* random, particle_t* particles);
// samples the stochastic torques from the Gausian PRNG
int util_particle_stochastic_torques(random_t* random, particle_t* particles);
// uses brute-force to obtain the resultant determintic force acting on the particles,
// the callback implements interaction between pairs of virtual particles; the `offsets'
// are used to consider interactions with the virtual particles (when the `offsets' are
// zero actual particles are considered)
void util_particle_brute_force(particle_t* particles,
			       void (*callback)(particle_t* particles,
						size_t const id,
						double const offset_x,
						double const offset_y,
						double const offset_z));

#endif

/*

OpenBDS							September 26, 2023

source: util/particle.h
author: @misael-diaz

Synopsis:
Provides prototypes for utils that operate on OBDS particle objects.

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
