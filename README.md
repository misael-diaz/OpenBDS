# OpenBDS
Open-source Brownian Dynamics Simulator OBDS.

Implements a Brownian Dynamics Simulator BDS in modern FORTRAN and C.
The higher level Object-Oriented Programming OOP code is written in FORTRAN and everything
else is written in C.

This is a work in progress, contributions are welcomed.

## Compilation

Instructions to compile the source code with GNU Make. Some degree of familiarity with the
command line is assumed.

Compilation requirements: GNU Make, GCC 10 (or higher), and a POSIX compliant OS
(Linux or Mac OS X).

You can compile the OBDS code from the source directory src:

```sh
make clean && make
```

It would be a great idea to edit src/make-inc, especially if you want to enable compiler
optimizations (for these are turned off by default) or use a different compiler.

The OBDS code has been successfully built with GCC and the Intel Compiler in Linux and
Mac OS X.

## Running OBDS tests

Minimal OBDS FORTRAN Test Code can be executed from the src directory via:

```sh
./test/particles-sphere/fortran-test-particles-sphere.bin
```

The code might complain if the run directory is missing, to create it issue the following
commands:

```sh
mkdir -p run/bds/data/params
```

```sh
mkdir -p run/bds/data/positions
```

The test just spawns the particles in the system box, tests the computation of the
interparticle forces, the application of the periodic boundaries, among other things.
It is not by any means a fully fledged BDS code, in fact, it only executes one step
and exits.

OBDS Test Code can be executed from the src directory via:

```sh
./test/particles-sphere/test-particles-sphere.bin
```

This test code can be used to perform Brownian Dynamics Simulations.

## Initialization Run Video

Click on the image below to watch the video on youtube.
[![Watch the video](https://img.youtube.com/vi/ykZwhjFEyho/hqdefault.jpg)](https://www.youtube.com/watch?v=ykZwhjFEyho)

## BDS Run Videos

Links to OBDS runs videos hosted on youtube:
[![Watch the video](https://img.youtube.com/vi/WmljeRStXR0/hqdefault.jpg)](https://www.youtube.com/watch?v=WmljeRStXR0)

[![Watch the video](https://img.youtube.com/vi/BdQRtJYWLe4/hqdefault.jpg)](https://www.youtube.com/watch?v=BdQRtJYWLe4)
