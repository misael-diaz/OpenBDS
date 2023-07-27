#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "sphere.h"
#include "system.h"
#include "util.h"

// defines the system size, or equivalently, the number of spheres
#define SIZE ( (size_t) NUM_SPHERES )

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
  return 0;
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

  mask_partition(numel, x, mask);

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

  mask_partition(numel, x, mask);
  mask_unlimited(numel, x, mask, temp, bitmask);

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

  mask_unlimited(numel, x, mask, temp, bitmask);

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
      if (dist > RANGE)
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
      bool const interacting = (dist <= RANGE);
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
  double* t = m + NUM_SPHERES;	// temporary (placeholder for intermediate computations)
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

      d[j] /= RANGE;
    }

    // generates bitmasks for the pairs (ones if interacting, zeros otherwise):

    inrange(d, t, m);

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
