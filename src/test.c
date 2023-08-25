#include <stdio.h>		// for console and file logging purposes
#include <stdint.h>		// used by xorshift() pseudo-random number generator
#include <math.h>		// needed by nrand() pseudo-random number generator
#include <time.h>		// provides system time(), used for seeding rand()
#include <sys/types.h>		// required by getpid(), we use the process id for seeding
#include <unistd.h>		// also required by getpid()
#include <string.h>		// for string comparison util strcmp()

#include "sphere.h"
#include "system.h"
#include "util.h"

// defines the system size, or equivalently, the number of spheres
#define SIZE ( (size_t) NUM_SPHERES )
#define FAILURE ( (int) 0xffffffff )
#define SUCCESS ( (int) 0x00000000 )
#define MIN_NUM_STEPS ( (size_t) 0x0000000000010000 )
#define TIME_STEP ( 1.0 / ( (double) MIN_NUM_STEPS ) )
#define CLAMP (0.0625 / TIME_STEP)
#define LOG true

typedef union
{
  uint64_t bin;
  double data;
} alias_t;

void test_init();
void test_partition_masking();
void test_unlimited_masking();
void test_pbc();
void test_list();
void test_list2();
void test_overlap();
void test_inrange();
void test_force();
void test_force2();
void test_force3();
void test_equilibration();
void test_xorshift64();
void test_nrand();
void test_sha512sum();
void test_info();
void test_bds();

void clamp (double* restrict force,
	    double* restrict tmp,
	    double* restrict temp,
	    double* restrict bitmask);

int main ()
{
  test_init();
  test_partition_masking();
  test_unlimited_masking();
  test_pbc();
  test_list();
  test_list2();
  test_overlap();
  test_inrange();
  test_equilibration();
  test_xorshift64();
  test_nrand();
  test_bds();
//disabled tests:
//test_force();		// we can safely disable this test at this point
//test_force2();	// disables the equilibration run for a pair of particles
//test_force3();	// disables the equilibration run for two pairs of particles
//test_sha512sum();	// uncomment if there's a positions.txt file to check
//test_info();		// we don't want to overwrite the BDS parameters file
  return 0;
}


// gets the sha512sum of stable.txt file
int getsha512sum (char* hash)
{
  FILE* file = fopen("stable.txt", "r");
  if (file == NULL)
  {
    printf("getsha512sum(): stable.txt file missing\n");
    return FAILURE;
  }
  fclose(file);

  FILE* pipe = popen("sha512sum stable.txt", "r");
  if (pipe == NULL)
  {
    printf("getsha512sum(): failed generate sha512sum\n");
    return FAILURE;
  }

  if (fscanf(pipe, "%s", hash) != 1)
  {
    pclose(pipe);
    printf("getsha512sum(): failed read sha512sum\n");
    return FAILURE;
  }

  pclose(pipe);
  return SUCCESS;
}


// dumps BDS parameters to a plain text file
int info ()
{
  char hash[256];
  if (getsha512sum(hash) == FAILURE)
  {
    return FAILURE;
  }

  FILE* file = fopen("params-bds.txt", "w");
  if (file == NULL)
  {
    printf("info(): IO ERROR\n");
    return FAILURE;
  }

  fprintf(file, "LIMIT:       %.16e\n", LIMIT);
  fprintf(file, "LENGTH:      %.16e\n", LENGTH);
  fprintf(file, "TIME_STEP:   %.16e\n", TIME_STEP);
  fprintf(file, "NUM_SPHERES: %lu\n",   SIZE);
  fprintf(file, "SHA512SUM:   %s\n",    hash);

  fclose(file);
  return SUCCESS;
}


// compares the keys of the BDS parameters file
int keys (const char* restrict key, const char* restrict str)
{
  return strcmp(key, str);
}


// checks the simulation parameters against the MACROS
int getinfo ()
{
  char hash[256];
  if (getsha512sum(hash) == FAILURE)
  {
    return FAILURE;
  }

  FILE* file = fopen("params-bds.txt", "r");
  if (file == NULL)
  {
    printf("getinfo(): IO ERROR\n");
    return FAILURE;
  }

  char key[32];
  char sha[256];
  double limit[1];
  double length[1];
  double time_step[1];
  size_t num_spheres[1];
  char const limit_key[] = "LIMIT:";
  char const length_key[] = "LENGTH:";
  char const time_step_key[] = "TIME_STEP:";
  char const num_spheres_key[] = "NUM_SPHERES:";
  char const sha512sum_key[] = "SHA512SUM:";
  if (fscanf(file, "%s %lf",  key, limit) != 2)
  {
    fclose(file);
    printf("getinfo(): unexpected IO ERROR while reading limit\n");
    return FAILURE;
  }

  if (keys(limit_key, key) != 0)
  {
    fclose(file);
    printf("getinfo(): expected key %s but got %s \n", limit_key, key);
    return FAILURE;
  }

  if (fscanf(file, "%s %lf",  key, length) != 2)
  {
    fclose(file);
    printf("getinfo(): unexpected IO ERROR while reading length\n");
    return FAILURE;
  }

  if (keys(length_key, key) != 0)
  {
    fclose(file);
    printf("getinfo(): expected key %s but got %s \n", length_key, key);
    return FAILURE;
  }

  if (fscanf(file, "%s %lf",  key, time_step) != 2)
  {
    fclose(file);
    printf("getinfo(): unexpected IO ERROR while reading time-step\n");
    return FAILURE;
  }

  if (keys(time_step_key, key) != 0)
  {
    fclose(file);
    printf("getinfo(): expected key %s but got %s \n", time_step_key, key);
    return FAILURE;
  }

  if (fscanf(file, "%s %lu", key, num_spheres) != 2)
  {
    fclose(file);
    printf("getinfo(): unexpected IO ERROR while reading the number of spheres\n");
    return FAILURE;
  }

  if (keys(num_spheres_key, key) != 0)
  {
    fclose(file);
    printf("getinfo(): expected key %s but got %s \n", num_spheres_key, key);
    return FAILURE;
  }

  if (fscanf(file, "%s %s", key, sha) != 2)
  {
    fclose(file);
    printf("getinfo(): unexpected IO ERROR while reading sha512sum\n");
    return FAILURE;
  }

  if (keys(sha512sum_key, key) != 0)
  {
    fclose(file);
    printf("getinfo(): expected key %s but got %s \n", sha512sum_key, key);
    return FAILURE;
  }

  if (*limit != LIMIT)
  {
    fclose(file);
    printf("getinfo(): wrong LIMIT\n");
    return FAILURE;
  }

  if (*length != LENGTH)
  {
    fclose(file);
    printf("getinfo(): wrong LENGTH\n");
    return FAILURE;
  }

  if (*time_step != TIME_STEP)
  {
    fclose(file);
    printf("getinfo(): wrong TIME_STEP\n");
    return FAILURE;
  }

  if (*num_spheres != SIZE)
  {
    fclose(file);
    printf("getinfo(): wrong NUM_SPHERES\n");
    return FAILURE;
  }

  if (strcmp(sha, hash) != 0)
  {
    fclose(file);
    printf("getinfo(): detected differing sha512sums\n");
    return FAILURE;
  }

  fclose(file);
  return SUCCESS;
}


// seeds the xorshif64() backed Pseudo Random Number Generator PRNG
void seed (uint64_t* state)
{
  size_t count = 0;			// initializes the tries counter
  srand( time(NULL) & getpid() );	// uses the time and process id to seed rand()
  uint64_t seed = rand();		// initializes the seed
  uint64_t const seed_default = (0xffffffffffffffff);
  // caters zero valued seeds, which render xorshift64() useless
  while (seed == 0)
  {
    if (count == 16)
    {
      printf("seed(): WARNING using default seed value\n");
      seed = seed_default;
      break;
    }
    seed = rand();
    ++count;
  }
  *state = seed;
}


// yields uniformly pseudo-random numbers in the asymmetric range [0, 1)
double urand (uint64_t* state)
{
  // implements Marsaglia's 64-bit xorshift pseudo-random number generator:
  uint64_t x = *state;
  x ^= (x << 13);
  x ^= (x >> 7);
  x ^= (x << 17);
  *state = x;

  // constructs the binary floating-point representation of 2^(-64):
  uint64_t const n = -64;					// exponent
  uint64_t const bias = 1023;					// bias
  alias_t const c = { .bin = ( (n + bias) << 52 ) };		// binary representation
  double const data = c.data;					// floating point value

  // scales the pseudo-random number so that the generator returns values in [0, 1):
  return ( data * ( (double) x ) );
}


// yields normally distributed pseudo-random numbers
double nrand (uint64_t* state)
{
  // implements Box-Muller's method:
  double x = INFINITY;
  double y = INFINITY;
  double r = INFINITY;
  while (r > 1.0)
  {
    x = 2.0 * urand(state) - 1.0;
    y = 2.0 * urand(state) - 1.0;
    r = (x * x) + (y * y);
  }
  r = sqrt( ( -2.0 * log(r) ) / r );
  x *= r;
  return x;
}


void test_init ()
{
  sphere_t* spheres = create();

  if (spheres == NULL)
  {
    return;
  }

  size_t const size = SIZE;
  const int64_t* id = spheres -> id;
  for (size_t i = 0; i != size; ++i)
  {
    printf("id: %ld \n", id[i]);
  }

  double f = 0;
  const double* f_x = spheres -> f_x;
  const double* f_y = spheres -> f_y;
  const double* f_z = spheres -> f_z;
  // sums the "net force" on the spheres (we expect zero owing to the initialization and
  // we are not computing the actual force magnitude via the squared root on purpose)
  for (size_t i = 0; i != size; ++i)
  {
    f += (f_x[i] * f_x[i]) + (f_y[i] * f_y[i]) + (f_z[i] * f_z[i]);
  }

  printf("f: %f \n", f);

  spheres = destroy(spheres);
}


void test_partition_masking ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double mask[numel];		// bitmask (non-zero for particles beyond system limits)

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  // scaling so that x ~ 1 (it's really not required to do the scaling for this test)
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] /= (0.5 * LENGTH);
  }

  size_t count = 0;
  // counts number of particles in left partition via signbit() MACRO
  for (size_t i = 0; i != numel; ++i)
  {
    if ( signbit(x[i]) )
    {
      ++count;
    }
  }

  // masks particles in the right partition so that we work on those in the left:

  mask_partition(x, mask);

  // counts the number of particles in the left partition to test the masking algorithm:

  alias_t* m = mask;
  for (size_t i = 0; i != numel; ++i)
  {
    m[i].bin &= 1;
  }

  size_t sum = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    sum += m[i].bin;
  }

  printf("mask-partition-test[0]: ");
  if (sum != count)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


// tests the application of periodic boundary conditions via bitwise operations
void test_unlimited_masking ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double temp[numel];		// array temporary
  double mask[numel];		// bitmask for partitioning
  double bitmask[numel];	// bitmask (non-zero for particles beyond system limits)
  double offset[numel];		// applies offset to bound unlimited particles

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  // scaling so that abs(x) = [0, 2.0) (note: the masking algorithm expects this scaling)
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] /= (0.5 * LENGTH);
  }

  // masks particles in the right partition:

  mask_partition(x, mask);
  mask_unlimited(x, mask, temp, bitmask);

  // applies periodic boundary conditions (for particles whose x < -1.0):

  alias_t* o = offset;
  const alias_t* b = bitmask;
  alias_t  l = { .data = 1.0 };
  for (size_t i = 0; i != numel; ++i)
  {
    o[i].bin = (b[i].bin & l.bin);
  }

  for (size_t i = 0; i != numel; ++i)
  {
    x[i] += offset[i];
  }

  // masks particles in the left partition:

  alias_t* m = mask;
  for (size_t i = 0; i != numel; ++i)
  {
    m[i].bin = ~m[i].bin;
  }

  mask_unlimited(x, mask, temp, bitmask);

  l.data = -1.0;
  for (size_t i = 0; i != numel; ++i)
  {
    o[i].bin = (b[i].bin & l.bin);
  }

  // applies periodic boundary conditions (for particles whose x > +1.0):

  for (size_t i = 0; i != numel; ++i)
  {
    x[i] += offset[i];
  }

  // counts the number of remaining unlimited particles (we expect none):

  size_t count = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    if (x[i] < -1.0 || x[i] > 1.0)
    {
      ++count;
    }
  }

  printf("mask-unlimited-test[0]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  // scales back so that x = [-LIMIT, +LIMIT] for all particles
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] *= (0.5 * LENGTH);
  }
}


void test_pbc ()
{
  size_t const numel = SIZE;	// number of elements (also number of particles)
  double x[numel];		// particles x-axis coordinates
  double temp[numel];		// array temporary
  double mask[numel];		// bitmask for partitioning
  double bitmask[numel];	// bitmask (non-zero for particles beyond system limits)

  double const x_min = -1.25 * LIMIT;
  double const x_max = +1.25 * LIMIT;
  // simulates the presence of (some) particles exceeding the system dimensions
  for (size_t i = 0; i != numel; ++i)
  {
    x[i] = x_min + ( (double) rand() / RAND_MAX ) * (x_max - x_min);
  }

  pbc(x, temp, mask, bitmask);

  // counts the number of remaining unlimited particles (we expect none):

  size_t count = 0;
  for (size_t i = 0; i != numel; ++i)
  {
    if (x[i] < -LIMIT || x[i] > +LIMIT)
    {
      ++count;
    }
  }

  printf("pbc-test[0]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


// test the neighbor-list generator (see comments at the end of the source code)
void test_list ()
{
  // x-axis coordinates of the particles (assumes a 1D system for simplicity)
  const double x[] = {	// here we assume an interaction range of 1.0
    0.0,		// does not interact with other particles (others are too far)
    2.0,		// interacts with the next particle
    3.0,		// does not interact with other particles other than the previous
    7.0,		// interacts with the next two particles (see comments at the end)
    8.0,		// interacts with the next and the previous particle
    9.0,		// does not interact with other particles other than the previous
    16.0		// does not interact with other particles
    // Note: interactions with previous particles are implied by Newton's third law
  };

  size_t const N = ( sizeof(x) / sizeof(double) );	// gets the number of particles

  // initializes the neighbor-list (assumes the particles to be too far to interact):

  int64_t list[N];
  for (size_t i = 0; i != N; ++i)
  {
    list[i] = -1;
  }

  // generates the neighbor-list (a list that links interacting pairs) with brute force:

  for (size_t i = 0; i != (N - 1); ++i)
  {
    for (size_t j = (i + 1); j != N; ++j)
    {
      double const d = (x[i] - x[j]) * (x[i] - x[j]);
      // the interaction range has been selected arbitrarily for the sake of this test;
      // this will depend on the type of interaction in the actual BDS code
      bool const interacting = (d <= 1.0);
      if (interacting)
      {
	list[i] = j;
	break;
      }
    }
  }

  // initializes the count of jth particles that interact with the ith particle to zero:

  int64_t count[N];
  for (size_t i = 0; i != N; ++i)
  {
    count[i] = 0;
  }

  // traverses the neighbor-list to check for its correctness:

  for (size_t i = 0; i != (N - 1); ++i)
  {
    // if the ith particle is not interacting there's nothing to do so consider the next
    bool const not_interacting = (list[i] == -1);
    if (not_interacting)
    {
      continue;
    }

    // we have asserted that the ith particle interacts with at least one other particle
    for (size_t j = (i + 1); j != N; ++j)
    {
      // increments the number of jth particles that interact with the ith particle
      ++count[i];

      // if the jth particle does not interact with other particles, we are done
      bool const not_interacting = (list[j] == -1);
      if (not_interacting)
      {
	break;
      }
    }
  }

  // counts the number of failures:

  size_t fails = 0;
  for (size_t i = 0; i != N; ++i)
  {
    // gets the index of the last particle that interacts with the ith particle
    int64_t const idx = (i + count[i]);
    // increment the failures counter if that's not the case
    if (list[idx] != -1)
    {
      ++fails;
    }
  }

  printf("list-test[0]: ");
  if (fails != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


// checks for particles overlapping with their images
size_t overlap (const double* x,
	      const double* y,
	      const double* z,
	      double const offset_x,
	      double const offset_y,
	      double const offset_z)
{
  size_t overlaps = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const r_x = (x[j] + offset_x);
      double const r_y = (y[j] + offset_y);
      double const r_z = (z[j] + offset_z);
      double const d_x = (x[i] - r_x);
      double const d_y = (y[i] - r_y);
      double const d_z = (z[i] - r_z);
      // computes the squared distance of the pair for speed
      double const d = (d_x * d_x) + (d_y * d_y) + (d_z * d_z);

      size_t const contact = 4.0;
      if (d < contact)
      {
	++overlaps;
      }
    }
  }

  return overlaps;
}


// checks if the particle-insertion algorithm (grid-based) produces overlaps
void test_overlap ()
{
  // the system is cubic so we need only worry about one dimension only
  size_t const len = LENGTH;
  // defines the number of spheres that fit at contact along any dimension
  size_t const count = (len / 2);
  size_t const count2 = (count * count);
  size_t const count3 = (count * count * count);

  // informs the user if it is impossible (with the current scheme) to allocate the
  // spheres in contact with one another in the space (volume) that the system has
  if (NUM_SPHERES > count3)
  {
    printf("overlap(): it is impossible to fit %d spheres in the system\n", NUM_SPHERES);
    return;
  }

  double pos[count];
  // defines the contact position of the spheres along any dimension
  for (size_t i = 0; i != count; ++i)
  {
    pos[i] = 1.0 + 2.0 * ( (double) i );
  }

  double x[NUM_SPHERES];
  double y[NUM_SPHERES];
  double z[NUM_SPHERES];
  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const i = (n % count);
    x[n] = pos[i];
  }

  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const j = (n % count2) / count;
    y[n] = pos[j];
  }

  for (size_t n = 0; n != NUM_SPHERES; ++n)
  {
    size_t const k = (n / count2);
    z[n] = pos[k];
  }

  // check for overlaps (we expect none):

  size_t overlaps = 0;
  for (size_t i = 0; i != (NUM_SPHERES - 1); ++i)
  {
    for (size_t j = (i + 1); j != NUM_SPHERES; ++j)
    {
      double const d_x = (x[i] - x[j]);
      double const d_y = (y[i] - y[j]);
      double const d_z = (z[i] - z[j]);
      // computes the squared distance of the pair for speed
      double const d = (d_x * d_x) + (d_y * d_y) + (d_z * d_z);

      size_t const contact = 4.0;
      if (d < contact)
      {
	++overlaps;
      }
    }
  }

  // checks for overlaps with the images (accounts for the periodicity of the system):

  double offset_x = 0.0;
  double offset_y = 0.0;
  double offset_z = 0.0;

  offset_x = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);


  // checks even the less likely images:


  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);


  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  offset_z = -LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);


  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = -LENGTH;
  offset_y = -LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = -LENGTH;
  offset_y = +LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = 0.0;
  offset_y = 0.0;
  offset_z = 0.0;

  offset_x = +LENGTH;
  offset_y = -LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);

  offset_x = +LENGTH;
  offset_y = +LENGTH;
  offset_z = +LENGTH;
  overlaps += overlap(x, y, z, offset_x, offset_y, offset_z);


  printf("overlap-test[0]: ");
  if (overlaps != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }


  // shift the particles so that their coordinates are in the range [-LIMIT, LIMIT]
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    y[i] -= LIMIT;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    z[i] -= LIMIT;
  }


  // checks that there are no particles beyond the system limits:


  size_t fails = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    if (x[i] < -LIMIT || x[i] > +LIMIT)
    {
      ++fails;
    }
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    if (y[i] < -LIMIT || y[i] > +LIMIT)
    {
      ++fails;
    }
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    if (z[i] < -LIMIT || z[i] > +LIMIT)
    {
      ++fails;
    }
  }


  printf("overlap-test[1]: ");
  if (overlaps != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }


  // uncomment if you wish to look at the coordinates
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    //printf("%+e %+e %+e\n", x[i], y[i], z[i]);
  }
}


void test_list2()
{
  const char fname[] = "positions.txt";
  FILE* file = fopen(fname, "r");
  if (file == NULL)
  {
    printf("IO ERROR with file: %s \n", fname);
    return;
  }

  double data[4 * NUM_SPHERES];
  double* x = data;
  double* y = x + NUM_SPHERES;
  double* z = y + NUM_SPHERES;
  double* d = z + NUM_SPHERES;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    int const numit = fscanf(file, "%lf %lf %lf \n", x, y, z);
    if (numit != 3)
    {
      fclose(file);
      // an error could happen if the number of spheres does not match the number of lines
      printf("test_list2(): unexpected IO Error with %s \n", fname);
      return;
    }
    ++x;
    ++y;
    ++z;
  }

  x = data;
  y = x + NUM_SPHERES;
  z = y + NUM_SPHERES;

  // counts the number of out-of-range (or non-interacting) spheres:

  size_t count = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      d[j] = (x[i] - x[j]) * (x[i] - x[j]) +
	     (y[i] - y[j]) * (y[i] - y[j]) +
	     (z[i] - z[j]) * (z[i] - z[j]);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      if (dist > RANGE2)
      {
	++count;
      }
    }
  }

  // `count' shouldn't be zero because the data (presumably) has more than one cluster:

  printf("list-test[1]: ");
  if (count == 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  // generates the neighbor-list:

  int64_t nlist[NUM_SPHERES];
  list(nlist, d, x, y, z);

  // checks if there are interacting spheres missing (or unlinked) in the neighbor-list:

  count = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      d[j] = (x[i] - x[j]) * (x[i] - x[j]) +
	     (y[i] - y[j]) * (y[i] - y[j]) +
	     (z[i] - z[j]) * (z[i] - z[j]);
    }

    int64_t const root_i = head(nlist, i);
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }
  }

  printf("list-test[2]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  count = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double offset_x = 0.0;
    double offset_y = 0.0;
    double offset_z = 0.0;

    offset_x = -LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }

    offset_x = 0.0;
    offset_y = 0.0;
    offset_z = 0.0;

    offset_x = +LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }

    offset_x = 0.0;
    offset_y = 0.0;
    offset_z = 0.0;

    offset_y = -LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }

    offset_x = 0.0;
    offset_y = 0.0;
    offset_z = 0.0;

    offset_y = +LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }

    offset_x = 0.0;
    offset_y = 0.0;
    offset_z = 0.0;

    offset_z = -LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }

    offset_x = 0.0;
    offset_y = 0.0;
    offset_z = 0.0;

    offset_z = +LENGTH;
    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const x_i = x[i];
      double const y_i = y[i];
      double const z_i = z[i];

      double const x_j = (x[j] + offset_x);
      double const y_j = (y[j] + offset_y);
      double const z_j = (z[j] + offset_z);
      d[j] = (x_i - x_j) * (x_i - x_j) +
	(y_i - y_j) * (y_i - y_j) +
	(z_i - z_j) * (z_i - z_j);
    }

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      double const dist = d[j];
      int64_t const root_i = head(nlist, i);
      int64_t const root_j = head(nlist, j);
      bool const interacting = (dist <= RANGE2);
      bool const unlinked = (root_i != root_j);
      if (interacting && unlinked)
      {
	++count;
      }
    }
  }

  printf("list-test[3]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  fclose(file);
}


void test_inrange ()
{
  const char fname[] = "positions.txt";
  FILE* file = fopen(fname, "r");
  if (file == NULL)
  {
    printf("IO ERROR with file: %s \n", fname);
    return;
  }

  double data[6 * NUM_SPHERES];
  double* x = data;		// x-position
  double* y = x + NUM_SPHERES;	// y-position
  double* z = y + NUM_SPHERES;	// z-position
  double* d = z + NUM_SPHERES;	// interparticle distance
  double* m = d + NUM_SPHERES;	// bitmask
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    int const numit = fscanf(file, "%lf %lf %lf \n", x, y, z);
    if (numit != 3)
    {
      fclose(file);
      // an error could happen if the number of spheres does not match the number of lines
      printf("test_list2(): unexpected IO Error with %s \n", fname);
      return;
    }
    ++x;
    ++y;
    ++z;
  }

  x = data;
  y = x + NUM_SPHERES;
  z = y + NUM_SPHERES;

  // counts the number of false positives (masked particles that do not really interact):

  size_t count = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {

    // scales the (squared) separation distance of the pairs (an inrange() requirement):

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      d[j] = (x[i] - x[j]) * (x[i] - x[j]) +
	     (y[i] - y[j]) * (y[i] - y[j]) +
	     (z[i] - z[j]) * (z[i] - z[j]);

      d[j] /= RANGE2;
    }

    // generates bitmasks for the pairs (ones if interacting, zeros otherwise):

    inrange(d, m);

    for (size_t j = 0; j != NUM_SPHERES; ++j)
    {
      const alias_t* mask = m;
      double const dist = d[j];
      bool const not_interacting = (dist >= 1.0);
      // checks if the pair is not interacting but inrange() says otherwise
      if (not_interacting && mask[i].bin)
      {
	++count;
      }

      // checks if we are properly masking the particle itself to avert (future) NaNs
      if (dist == 0 && mask[i].bin)
      {
	++count;
      }

      // checks if the distance is non-zero when the pair is the same particle
      if (i == j && dist != 0)
      {
	++count;
      }
    }
  }

  printf("inrange-test[0]: ");
  if (count != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  fclose(file);
}


// shows that the force is zero for non-interacting particles
void test_force ()
{
  double d[NUM_SPHERES];
  double r[NUM_SPHERES];
  double f[NUM_SPHERES];
  double t[NUM_SPHERES];
  double res[NUM_SPHERES];
  double mask[NUM_SPHERES];

  zeros(d);
  zeros(r);
  zeros(f);
  zeros(mask);

  r[0] = 0.50;
  r[1] = 1.00;
  r[2] = 1.50;
  r[3] = 2.00;
  r[4] = 2.50;
  r[5] = 2.75;
  r[6] = 3.00;
  r[7] = 4.00;
  r[8] = 16.0;
  r[9] = 64.0;

  // we assume that the ith particle is located at the origin and that the jth particles
  // are at a distance d = (d_i - d_j) from the ith particle (hence the negative sign)
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    d[i] = -r[i];
  }

  // note that we should not expect r to retain its values after calling force() because
  // it is used as a placeholder for intermediate computations (the benefit of doing so
  // is not evident at this point but it shall be later)
  force(r, f, mask);

  for (size_t i = 0; i != 16; ++i)
  {
    double const force = f[i] * d[i];
    res[i] = force;
  }

  // clamps the force so that it does not exceeds its maximum value `CLAMP'
  clamp(res, r, t, mask);

  for (size_t i = 0; i != 16; ++i)
  {
    printf("r: %e f: %+e \n", -d[i], res[i]);
  }
}


// checks that a pair of particles reach the equilibrium distance as time progresses,
// the equilibrium distance is equal to the interaction range and it is approached
// asymptotically
void test_force2 ()
{
  // we need these arrays because the utilities expect arrays of size NUM_SPHERES
  double x[NUM_SPHERES];		// position array
  double r[NUM_SPHERES];		// placeholder
  double f[NUM_SPHERES];		// force
  double mask[NUM_SPHERES];		// placeholder for the bitmask

  // sets the `fixed' particle at the origin along others which are not considered at all
  zeros(x);
  zeros(r);
  zeros(f);
  zeros(mask);

  // sets the `free' particle at contact with the fixed particle
  x[1] = CONTACT;

  // updates the position of the `free' particle:

  double const dt = TIME_STEP;
  size_t const steps = 16 * MIN_NUM_STEPS;
  for (size_t step = 0; step != steps; ++step)
  {
    // gets the distance between the fixed and free particles
    double const dist = (x[1] - x[0]);
    // sets the distance in the designated placeholder
    r[1] = dist;
    force(r, f, mask);
    // updates position of the `free' particle via Euler's method (force is f[1] * dist):
    x[1] = x[1] + dt * (f[1] * dist);

    if (step % (steps / 16) == 0)
    {
      // logs the position and the interaction force (shows expected behavior)
      printf("x: %e f: %+e \n", x[1], (f[1] * dist));
    }
  }
}


void shift (double* restrict x, const double* restrict f_x)
{
  double const dt = TIME_STEP;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] += (dt * f_x[i]);
  }
}


void updates (double* restrict x,
	      double* restrict y,
	      double* restrict z,
	      const double* restrict f_x,
	      const double* restrict f_y,
	      const double* restrict f_z)
{
  shift(x, f_x);
  shift(y, f_y);
  shift(z, f_z);
}


// gets the net force on the ith particle by considering its interactions with the
// jth particles
void pairs (size_t const i,
	    const double* restrict x,
	    const double* restrict y,
	    const double* restrict z,
	    double const offset_x,
	    double const offset_y,
	    double const offset_z,
	    double* restrict f_x,
	    double* restrict f_y,
	    double* restrict f_z,
	    double* restrict f,
	    double* restrict d,
	    double* restrict mask)
{
  // computes the interparticle distance of the ith and jth particles:

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    double const d_x = ( x[i] - (x[j] + offset_x) );
    double const d_y = ( y[i] - (y[j] + offset_y) );
    double const d_z = ( z[i] - (z[j] + offset_z) );
    double const d2 = d_x * d_x + d_y * d_y + d_z * d_z;
    d[j] = d2;
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    d[j] = sqrt(d[j]);
  }

  // gets the force to distance ratio, F / r
  force(d, f, mask);

  // calculates the force components:

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    d[j] = ( x[i] - (x[j] + offset_x) );
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    mask[j] = f[j] * d[j];
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    f_x[i] += mask[j];
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    d[j] = ( y[i] - (y[j] + offset_y) );
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    mask[j] = f[j] * d[j];
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    f_y[i] += mask[j];
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    d[j] = ( z[i] - (z[j] + offset_z) );
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    mask[j] = f[j] * d[j];
  }

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    f_z[i] += mask[j];
  }
}


// gets the net (or resultant) force on the ith particles
void resultant (const double* restrict x,
		const double* restrict y,
		const double* restrict z,
		double* restrict f_x,
		double* restrict f_y,
		double* restrict f_z,
		double* restrict f,
		double* restrict d,
		double* restrict mask)
{
  double const offset_x = 0;
  double const offset_y = 0;
  double const offset_z = 0;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, f, d, mask);
  }
}


// as resultant() but with periodic boundaries considerations
void resultant2(const double* restrict x,
		const double* restrict y,
		const double* restrict z,
		double* restrict f_x,
		double* restrict f_y,
		double* restrict f_z,
		double* restrict tmp,
		double* restrict temp,
		double* restrict mask)
{
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = -LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

//for (size_t i = 0; i != NUM_SPHERES; ++i)
//{
//  double const offset_x = 0;
//  double const offset_y = 0;
//  double const offset_z = 0;
//  pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
//}

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = 0;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = -LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = 0;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }


  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = -LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = 0;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const offset_x = +LENGTH;
    double const offset_y = +LENGTH;
    double const offset_z = +LENGTH;
    pairs(i, x, y, z, offset_x, offset_y, offset_z, f_x, f_y, f_z, tmp, temp, mask);
  }
}


void test_force3 ()
{
  double x[NUM_SPHERES];		// x position array
  double y[NUM_SPHERES];		// y position array
  double z[NUM_SPHERES];		// z position array
  double d[NUM_SPHERES];		// distance
  double f_x[NUM_SPHERES];		// x component of the force
  double f_y[NUM_SPHERES];		// y component of the force
  double f_z[NUM_SPHERES];		// z component of the force
  double f[NUM_SPHERES];		// magnitude of the force
  double mask[NUM_SPHERES];		// placeholder for the bitmask

  zeros(x);
  zeros(y);
  zeros(z);
  zeros(d);
  zeros(f);
  zeros(mask);

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] = LENGTH;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    y[i] = LENGTH;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    z[i] = LENGTH;
  }

  x[0] = 0.0;
  y[0] = 0.0;
  z[0] = 0.0;

  x[1] = CONTACT;
  y[1] = 0.0;
  z[1] = 0.0;

  x[2] = 0.0;
  y[2] = CONTACT;
  z[2] = 0.0;

  x[3] = CONTACT;
  y[3] = CONTACT;
  z[3] = 0.0;

  size_t const steps = 65536;
  for (size_t step = 0; step != steps; ++step)
  {
    // initializes the net force on the ith particles

    zeros(f_x);
    zeros(f_y);
    zeros(f_z);

    // computes the net force on the ith particles

    resultant(x, y, z, f_x, f_y, f_z, f, d, mask);

    // updates the particle positions

    updates(x, y, z, f_x, f_y, f_z);
  }

  // logs the final particle positions:

  printf("(x, y, z): %+e %+e %+e \n", x[0], y[0], z[0]);
  printf("(x, y, z): %+e %+e %+e \n", x[1], y[1], z[1]);
  printf("(x, y, z): %+e %+e %+e \n", x[2], y[2], z[2]);
  printf("(x, y, z): %+e %+e %+e \n", x[3], y[3], z[3]);

  // logs the distances relative to the first particle (id = 0):

  for (size_t j = 0; j != NUM_SPHERES; ++j)
  {
    double const d_x = (x[0] - x[j]);
    double const d_y = (y[0] - y[j]);
    double const d_z = (z[0] - z[j]);
    double const d2 = d_x * d_x + d_y * d_y + d_z * d_z;
    d[j] = sqrt(d2);
  }

  for (size_t j = 0; j != 4; ++j)
  {
    printf("dist: %e \n", d[j]);
  }
}


void pbcs(double* restrict x,
	  double* restrict y,
	  double* restrict z,
	  double* restrict tmp,
	  double* restrict temp,
	  double* restrict mask)
{
    pbc(x, tmp, temp, mask);
    pbc(y, tmp, temp, mask);
    pbc(z, tmp, temp, mask);
}


// logs the current (determined by the step id) particle positions to a plain text file
int logger (const sphere_t* spheres, const size_t step)
{
  char fname[256];
  sprintf(fname, "run/equilibration/data/positions/positions-%032lu.txt", step);
  FILE* file = fopen(fname, "w");
  if (file == NULL)
  {
    return FAILURE;
  }

  const double* x = spheres -> x;
  const double* y = spheres -> y;
  const double* z = spheres -> z;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    const char format[] = "%+.16e %+.16e %+.16e\n";
    fprintf(file, format, x[i], y[i], z[i]);
  }

  fclose(file);
  return SUCCESS;
}


// this test is a system equilibration run (without Brownian motion)
void test_equilibration ()
{
  // sets the particles at grid locations (or lattice structure):

  sphere_t* spheres = create();
  if (LOG)
  {
    if (logger(spheres, 0) == FAILURE)
    {
      const char exports[] = "run/equilibration/data/positions/";
      printf("test-equilibration(): data exports directory %s does not exist\n", exports);
      spheres = destroy(spheres);
      return;
    }
  }

  double* x = spheres -> x;
  double* y = spheres -> y;
  double* z = spheres -> z;
  double* f_x = spheres -> f_x;
  double* f_y = spheres -> f_y;
  double* f_z = spheres -> f_z;
  double* f = spheres -> tmp;
  double* d = spheres -> temp;
  double* mask = spheres -> mask;

  // performs an equilibration run:

  bool failed = false;
  size_t const steps = 16 * MIN_NUM_STEPS;
  for (size_t step = 0; step != steps; ++step)
  {
    if (LOG)
    {
      if (step % 16 == 0)
      {
	logger(spheres, step);
      }
    }

    // zeroes the net force on particles:

    zeros(f_x);
    zeros(f_y);
    zeros(f_z);

    // computes the net force on the particles (considers periodicity):

    resultant2(x, y, z, f_x, f_y, f_z, f, d, mask);

    // gets the force along the axes for logging purposes

    double force = 0;
    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      force += (f_x[i] * f_x[i]);
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      force += (f_y[i] * f_y[i]);
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      force += (f_z[i] * f_z[i]);
    }

    // updates the particle positions

    updates(x, y, z, f_x, f_y, f_z);

    // applies periodic boundary conditions to the positions of the particles:

    pbcs(x, y, z, f, d, mask);

    // checks for out-of-bounds instances:

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (x[i] < -LIMIT || x[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (y[i] < -LIMIT || y[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (z[i] < -LIMIT || z[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    if (failed)
    {
      break;
    }

    // checks if the positions are close to their final equilibrium positions:

    force /= ( (3.0 * ( (double) NUM_SPHERES) ) );
    force = sqrt(force);

    double const tol = 7.450580596923828e-09;
    if (force < tol)
    {
      break;
    }

    // logs the average force along the axes on the console:

    if (step % 256 == 0)
    {
      printf("step: %lu force: %e \n", step, force);
    }
  }

  printf("equilibration-test[0]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  if (failed)
  {
    spheres = destroy(spheres);
    return;
  }

  // exports the particle positions for post-processing:

  const char fname[] = "stable.txt";
  FILE* file = fopen(fname, "w");
  if (file == NULL)
  {
    printf("IO ERROR with file %s\n", fname);
    spheres = destroy(spheres);
    return;
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    fprintf(file, "%+.15e %+.15e %+.15e\n", x[i], y[i], z[i]);
  }

  fclose(file);

  // dumps BDS equilibration run info to a plain text file:

  if (info() == FAILURE)
  {
    printf("WARNING: failed to dump BDS run info\n");
  }

  spheres = destroy(spheres);
}


// checks only if xorshift64() returns values in the asymmetric range [0, 1)
void test_xorshift64 ()
{
  size_t fails = 0;
  int64_t state[] = { 0xffffffffffffffff };		// -1
//uint64_t const period = 0xffffffffffffffff;		// 2^64 - 1
//printf("xorshift64() period: %lu \n", period);
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const r = xorshift64(state);
    //printf("r: %f \n", r);
    if (r >= 1.0)
    {
      ++fails;
    }
  }

  printf("xorshift64-test[0]: ");
  if (fails != 0)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


// tests the statistics of the normally distributed pseudo-random number generator;
// this is too simple a test but a test for checking if nrand() is not quite right
void test_nrand ()
{
  double avg = 0;				// average
  double std = 0;				// standard-deviation
  uint64_t state[] = { 0xffffffffffffffff };	// xorshift64() initial seed
  seed(state);					// attempts to improve the seed
  // could have stored the pseudo-random numbers in an array but this approach suffices
  // to get an estimate of the standard deviation since the average is close to zero
  size_t const size = (SIZE * SIZE);
  for (size_t i = 0; i != size; ++i)
  {
    double const r = nrand(state);
    avg += r;
    std += (r * r);
  }

  printf("avg (should be close to zero): %f \n", avg / size);
  printf("std (should be close to one): %f \n", sqrt( std / (size - 1) ) );
}


// tests getting the sha512sum of positions.txt
void test_sha512sum ()
{
  FILE* pipe = popen("sha512sum positions.txt", "r");
  if (pipe == NULL)
  {
    printf("test_sha512sum(): failed read sha512sum\n");
    return;
  }

  char sha[256];
  fscanf(pipe, "%s", sha);

//printf("%s\n", sha);

  pclose(pipe);
  printf("test-sha512sum(): succeeded in getting the sha512sum of positions.txt\n");
}


void force_stochastic (uint64_t* restrict state, double* restrict f_x)
{
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    f_x[i] = nrand(state);
  }
}


void shift_stochastic (double* restrict x, const double* restrict f_x)
{
  double const dt = TIME_STEP;
  double const mobility = sqrt(2.0 * dt);
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    x[i] += (mobility * f_x[i]);
  }
}


// computes the stochastic forces
void forces_stochastic (uint64_t* state, double* f_x, double* f_y, double* f_z)
{
  force_stochastic(state, f_x);
  force_stochastic(state, f_y);
  force_stochastic(state, f_z);
}


// shifts the particle positions by the effect of the Stochastic forces
void updates_stochastic(double* restrict x,
			double* restrict y,
			double* restrict z,
			const double* restrict f_x,
			const double* restrict f_y,
			const double* restrict f_z)
{
  shift_stochastic(x, f_x);
  shift_stochastic(y, f_y);
  shift_stochastic(z, f_z);
}


// gets the particle positions
int getpos (double* x, double* y, double* z)
{
  const char fname[] = "stable.txt";
  FILE* file = fopen(fname, "r");
  if (file == NULL)
  {
    printf("test-bds(): IO ERROR with file %s", fname);
    return FAILURE;
  }

  // iterators:

  double* xit = x;
  double* yit = y;
  double* zit = z;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    int const numit = fscanf(file, "%lf %lf %lf \n", xit, yit, zit);
    if (numit != 3)
    {
      fclose(file);
      printf("test-bds(): unexpected IO Error with %s \n", fname);
      return FAILURE;
    }
    ++xit;
    ++yit;
    ++zit;
  }

  fclose(file);

  return SUCCESS;
}


double minval (const double* x)
{
  double min = INFINITY;
  for (size_t i = 0; i != SIZE; ++i)
  {
    if (x[i] < min)
    {
      min = x[i];
    }
  }
  return min;
}


double maxval (const double* x)
{
  double max = -INFINITY;
  for (size_t i = 0; i != SIZE; ++i)
  {
    if (x[i] > max)
    {
      max = x[i];
    }
  }
  return max;
}


// this method takes into account how vectors are stored in spheres
double vmax (const double* x)
{
  double max = -INFINITY;
  for (size_t i = 0; i != (3 * SIZE); ++i)
  {
    if (x[i] > max)
    {
      max = x[i];
    }
  }
  return max;
}


// gets the 11-bits that comprise the exponent of a double precision floating-point number
uint64_t get_exp (uint64_t const x)
{
  return ( (x >> 52) & 0x7ff );
}


// yields bitmask of ones if abs(x) < 1, zeros otherwise
uint64_t fbitmask (uint64_t const x)
{
  uint64_t const m = ~(0x3ff);
  // masks the "high" bits that characterize 64-bit floats with exponent n > 0:
  uint64_t const hi =
    ( ( ( ( ( ( (get_exp(x) & m) & 0x7ff ) + (m & 0x7ff) ) & 0x800 ) >> 11 ) + 1 ) & 1 );
  // masks the "low" bits that characterize 64-bit floats with exponents n < 0:
  uint64_t const lo =
    ( ( ( ( ( (get_exp(x) & 0x3ff) ^ 0x3ff ) & 0x3ff ) + 0x3ff ) & 0x400 ) >> 10 );
  // Note: there are 64-bit floats with "low" bits set with exponent n > 0, so we have to
  // bitwise AND to make sure sure that the "high" bits are not set (that is, that x < 1).
  return ( (hi & lo) );
}


uint64_t twos_complement (uint64_t const x)
{
  return (~x + 1);
}


void mask_force (const double* restrict force, double* restrict bitmask)
{
  alias_t* b = bitmask;
  const alias_t* fp = force;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    b[i].bin = twos_complement( fbitmask(fp[i].bin) );
  }
}


void clamp (double* restrict force,
	    double* restrict tmp,
	    double* restrict temp,
	    double* restrict bitmask)
{
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    double const c = (1.0 / CLAMP);
    temp[i] = c * force[i];
  }

  mask_force(temp, bitmask);

  alias_t* t = temp;
  alias_t* f = force;
  alias_t* b = bitmask;
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    t[i].bin = (f[i].bin & b[i].bin);
  }

  alias_t* a = tmp;
  alias_t const max = { .data = CLAMP };
  // clamps with the maximum force in the same sense as the (original) force
  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    uint64_t const MSBMASK = 0x8000000000000000;
    a[i].bin = ( (f[i].bin & MSBMASK) | ( max.bin & (~b[i].bin) ) );
  }

  for (size_t i = 0; i != NUM_SPHERES; ++i)
  {
    force[i] = (tmp[i] + temp[i]);
  }
}


// gets the Mean Squared Displacement from the particle positions at time t and t + dt
double MSD (const double* restrict r1, const double* restrict r2, double* restrict msd)
{
  size_t const size = NUM_SPHERES;
  const double* x1 = r1;
  const double* y1 = x1 + size;
  const double* z1 = y1 + size;
  const double* x2 = r2;
  const double* y2 = x2 + size;
  const double* z2 = y2 + size;

  for (size_t i = 0; i != size; ++i)
  {
    msd[i] = (x1[i] - x2[i]) * (x1[i] - x2[i]) +
	     (y1[i] - y2[i]) * (y1[i] - y2[i]) +
	     (z1[i] - z2[i]) * (z1[i] - z2[i]);
  }

  double ret = 0;
  for (size_t i = 0; i != size; ++i)
  {
    ret += msd[i];
  }

  ret /= ( 3.0 * ( (double) NUM_SPHERES ) );
  return ret;
}


// conducts a BDS test run
void test_bds ()
{
  if (getinfo() == FAILURE)
  {
    return;
  }

  // seeds the xorshift64() prng
  uint64_t state[] = { 0xffffffffffffffff };
  seed(state);

  sphere_t* spheres = create();

  if (LOG)
  {
    if (logger(spheres, 0) == FAILURE)
    {
      const char exports[] = "run/equilibration/data/positions/positions";
      printf("test-bds(): data exports directory %s does not exist\n", exports);
      spheres = destroy(spheres);
      return;
    }
  }

  double* x = spheres -> x;
  double* y = spheres -> y;
  double* z = spheres -> z;
  double* r_x = spheres -> r_x;
  double* r_y = spheres -> r_y;
  double* r_z = spheres -> r_z;
  double* f_x = spheres -> f_x;
  double* f_y = spheres -> f_y;
  double* f_z = spheres -> f_z;
  double* t_x = spheres -> t_x;
  double* t_y = spheres -> t_y;
  double* t_z = spheres -> t_z;
  double* f = spheres -> tmp;
  double* d = spheres -> temp;
  double* mask = spheres -> mask;

  if (getpos(x, y, z) == FAILURE)
  {
    spheres = destroy(spheres);
    return;
  }

  // executes the BDS integrator

  double msd = 0;
  bool failed = false;
  size_t const steps = 16 * MIN_NUM_STEPS;
  for (size_t step = 0; step != steps; ++step)
  {
    if (LOG)
    {
      if (step % 16 == 0)
      {
	logger(spheres, step);
      }
    }

    // zeroes the net force on the particles:

    zeros(f_x);
    zeros(f_y);
    zeros(f_z);

    // stores the current (unbounded) positions for the MSD computation:

    t_x = r_x;
    t_y = r_y;
    t_z = r_z;

    // updates the particle positions by the action of the determinstic forces:

    resultant2(x, y, z, f_x, f_y, f_z, f, d, mask);
    clamp(f_x, f, d, mask);
    clamp(f_y, f, d, mask);
    clamp(f_z, f, d, mask);
    updates(x, y, z, f_x, f_y, f_z);
    updates(r_x, r_y, r_z, f_x, f_y, f_z);

    // logs the maximum deterministic force:

    /*
    if (step % 256 == 0)
    {
      const double* f = f_x;
      const double* vector = f;
      const char fmt[] = "step: %lu max-interaction-force: %.16e \n";
      printf(fmt, step, vmax(vector) );
    }
    */

    if (vmax(f_x) == CLAMP)
    {
      printf("test-bds(): clamped force detected in step %lu\n", step);
    }

    // updates the particle positions by the action of the Stochastic forces:

    forces_stochastic(state, f_x, f_y, f_z);
    updates_stochastic(x, y, z, f_x, f_y, f_z);
    updates(r_x, r_y, r_z, f_x, f_y, f_z);

    // applies periodic boundary conditions to the positions of the particles:

    pbcs(x, y, z, f, d, mask);

    // logs the Mean Squared Displacement MSD:

    msd += MSD(t_x, r_x, f);

    // logs the maximum stochastic force:

    /*
    if (step % 256 == 0)
    {
      const double* f = f_x;
      const double* vector = f;
      const char fmt[] = "step: %lu max-stochatic-force: %.16e \n";
      printf(fmt, step, vmax(vector) );
    }
    */

    // checks for out-of-bounds instances:

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (x[i] < -LIMIT || x[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (y[i] < -LIMIT || y[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    for (size_t i = 0; i != NUM_SPHERES; ++i)
    {
      if (z[i] < -LIMIT || z[i] > +LIMIT)
      {
	failed = true;
	break;
      }
    }

    if (failed)
    {
      printf("xmin: %+.16e xmax: %+.16e \n", minval(x), maxval(x));
      printf("ymin: %+.16e ymax: %+.16e \n", minval(y), maxval(y));
      printf("zmin: %+.16e zmax: %+.16e \n", minval(z), maxval(z));
      break;
    }
  }

  printf("bds-test[0]: ");
  if (failed)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  if (failed)
  {
    spheres = destroy(spheres);
    return;
  }

  if (LOG)
  {
    if (steps % 16 == 0)
    {
      logger(spheres, steps);
    }
  }

  spheres = destroy(spheres);
}


// tests dumping BDS info
void test_info ()
{
  // dumps the info:

  printf("dump-bds-info-test[0]: ");
  if (info() == FAILURE)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }

  if (info() == FAILURE)
  {
    return;
  }

  // checks the info:

  printf("dump-bds-info-test[1]: ");
  if (getinfo() == FAILURE)
  {
    printf("FAIL\n");
  }
  else
  {
    printf("PASS\n");
  }
}


/*

OpenBDS								July 19, 2023

source: test.c
author: @misael-diaz

Synopsis:
Tests the lower-level code that supports the Brownian Dynamics Simulator BDS.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] A Koenig and B Moo, Accelerated C++ Practical Programming by Example.
[1] MP Allen and DJ Tildesley, Computer Simulation of Liquids.
[2] S Kim and S Karrila, Microhydrodynamics: Principles and Selected Applications.

*/


// COMMENTS:
//
// Neighbor-List:
// The used neighbor list is efficient for dilute and semi-dilute systems where only
// a few pairs (or perhaps a triplet) are interacting. For denser systems the list will
// include (nearly all if not) all the particles even if there are pairs that are actually
// too far to be really interacting. This is the case because of the `implied assumption'
// that the ith particle interacts will all the neighbors of the jth particle. I might
// add other more efficient algorithms (with their respective data structures) later.
//
// Note that the test does not take into account the periodic boundaries for simplicity;
// later I shall add more tests that take this into consideration.


// TODO:
// [x] consider all possible images when computing the interparticle forces
