#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "util/random.h"
#include "util/type.h"

#define ABS(x) ( (x < 0)? -x : x )
#define SIZE_RANDOM sizeof(struct random)
#define SIZE_GENERATOR sizeof(struct generator)
#define SIZE_COUNT sizeof(size_t)
#define SIZE_STATE sizeof(uint64_t)
#define SIZE (SIZE_RANDOM + SIZE_GENERATOR + SIZE_COUNT + SIZE_STATE)
#define NUMEL ( (size_t) 0x0000000001000000 )
#define TOL 0.001953125
#define LOG false

extern struct util const util;

void test();

int main ()
{
  test();
  return 0;
}


double mean (const double* x)
{
  double mean = 0;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    mean += x[i];
  }
  mean /= ( (double) NUMEL );
  return mean;
}


double std (const double* x)
{
  double std = 0;
  double const avg = mean(x);
  for (size_t i = 0; i != NUMEL; ++i)
  {
    std += (x[i] - avg) * (x[i] - avg);
  }
  std /= ( (double) (NUMEL - 1) );
  return sqrt(std);
}


void test ()
{
  // NOTE: if compilation fails replace static_assert() calls with _Static_assert()
  static_assert(sizeof(struct random) == 16);
  static_assert(sizeof(struct generator) == 32);
  static_assert(sizeof(uint64_t) == 8);
  static_assert(sizeof(size_t) == 8);
  static_assert(sizeof(size_t) == 8);
  static_assert(SIZE == 64);
  size_t const size = SIZE;

  void* data = malloc(size);
  if (data == NULL)
  {
    return;
  }

  void* iter = data;
  struct random* random = (struct random*) iter;
  iter += sizeof(struct random);
  random -> generator = (struct generator*) iter;
  iter += sizeof(struct generator);
  random -> generator -> count = (size_t*) iter;
  iter += sizeof(size_t);
  random -> generator -> state = (uint64_t*) iter;
  iter += sizeof(uint64_t);

  int const stat = util.random.initializer(random, NRAND);
  if (stat != 0)
  {
    free(data);
    data = NULL;
    fprintf(stderr, "test-random(): PRNG seeding error\n");
    return;
  }

  double* prns = malloc( NUMEL * sizeof(double) );
  if (prns == NULL)
  {
    free(data);
    data = NULL;
    fprintf(stderr, "test-random(): memory error %s\n", strerror(errno));
    return;
  }

  double* prn = prns;
  for (size_t i = 0; i != NUMEL; ++i)
  {
    *prn = random -> fetch(random);
    ++prn;
  }

  double avg = mean(prns);
  avg = ABS(avg);
  double const stdev = std(prns);
  if (LOG)
  {
    printf("avg: %.16e\n", avg);
    printf("std: %.16e\n", stdev);
  }

  double const tol = TOL;
  printf("test-random[0]: ");
  if (avg < tol)
  {
    printf("PASS\n");
  }
  else
  {
    printf("FAIL\n");
  }

  printf("test-random[1]: ");
  if (ABS(1.0 - stdev) < tol)
  {
    printf("PASS\n");
  }
  else
  {
    printf("FAIL\n");
  }

  free(data);
  free(prns);
  data = NULL;
  prns = NULL;
}
