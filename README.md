# OpenBDS
Open-source Brownian Dynamics Simulator OBDS.

Implements a Brownian Dynamics Simulator BDS in modern FORTRAN and C.
The higher level Object-Oriented Programming OOP code is written in FORTRAN and everything
else is written in C.

This is a work in progress, contributions are welcomed.

## Compilation

Instructions to compile the source code with GNU Make. Some degree of familiarity with the
command line is assumed.

Compilation requirements: GNU [Make](https://www.gnu.org/software/make/),
[GCC](https://gcc.gnu.org/) 10 (or higher), and a
[POSIX](https://www.opengroup.org/austin/papers/backgrounder.html) compliant Operative
System OS. The code has been tested in GNU/Linux and Mac OS X. It might be possible to
compile the source in Windows under [Cygwin](https://www.cygwin.com/) but have yet to
test it myself.

You can compile the OBDS code by issuing the following commands on the terminal (at the
top level of the repository):

```sh
make clean && make
```

It would be a great idea to edit make-inc, the one at the top level of the repository,
especially if you want to enable compiler optimizations (for these are turned off by
default) or use a different compiler.

The OBDS code has been successfully built with the GNU C and FORTRAN Compilers `gcc` and
`gfortran`, the LLVM C Compiler `clang`, and the Intel C and FORTRAN Compilers `icc` and
`ifort` in both GNU/Linux and Mac OS X.

## Set Up the Execution Environment

Create the output directories that the OBDS app expects:

```sh
mkdir -p run/bds/data/params
```

```sh
mkdir -p run/bds/data/positions
```

```sh
mkdir -p run/render/frames
```

Note that these directories must be present in your current working directory.

## Executing the OBDS app

To execute the OBDS app issue the following command on the terminal:

```sh
./api/clang/src/bds/fortran/OpenBDS.bin
```

If you prefer to execute the OBDS code from Python change your working directory via

```sh
cd api/clang/src/bds/python
```

and then execute the code with the Python 3 interpreter:

```sh
python3 OpenBDS.py
```

contrary to its FORTRAN counterpart this one prepares the execution environment for you.

## Running OBDS tests

Minimal OBDS FORTRAN Test Code can be executed from the top level of the repository:

```sh
./api/clang/src/test/particles-sphere/fortran-test-particles-sphere.bin
```

The test just spawns the particles in the system box, tests the computation of the
interparticle forces, the application of the periodic boundaries, among other things.
It is not by any means a fully fledged BDS code, in fact, it only executes one step
and exits.

OBDS Test Code can be executed from the top level of the repository:

```sh
./api/clang/src/test/particles-sphere/test-particles-sphere.bin
```

This test code can be used to perform Brownian Dynamics Simulations.

## Initialization Run Video

Click on the image below to watch the video on youtube.
[![Watch the video](https://img.youtube.com/vi/ykZwhjFEyho/hqdefault.jpg)](https://www.youtube.com/watch?v=ykZwhjFEyho)

## BDS Run Videos

Links to OBDS runs videos hosted on youtube:
[![Watch the video](https://img.youtube.com/vi/WmljeRStXR0/hqdefault.jpg)](https://www.youtube.com/watch?v=WmljeRStXR0)

[![Watch the video](https://img.youtube.com/vi/BdQRtJYWLe4/hqdefault.jpg)](https://www.youtube.com/watch?v=BdQRtJYWLe4)
