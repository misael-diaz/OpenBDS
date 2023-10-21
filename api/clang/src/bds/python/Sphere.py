#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             September 22, 2023

source: Sphere.py
author: @misael-diaz

Synopsis:
Defines the Python OBDS Sphere class.

This code is experimental and prone to error, especially if it has not been maintained as
the project evolves. It goes without saying that any change to the header files will most
likely break this code since it is oblivious of their actual content.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] R Johansson, Numerical Python: Scientific Computing and Data
    Science Applications with NumPy, SciPy, and Matplotlib, 2nd edition
[1] https://docs.python.org/3/library/ctypes.html
"""

import os
import sys
import ctypes
import platform
from numpy import log2
from PRNG import c_random_t
from PRNG import c_generator_t
from OBDS_sphere import c_OBDS_Sphere_t
from OBDS_sphere import c_sphere_t
from OBDS_sphere import c_prop_t

if os.name != 'posix':
  errmsg = 'the Operative System OS must be POSIX compliant to be able run this code'
  raise OSError(errmsg)

if platform.system() == 'Linux':
  GLIBC = 'libc.so.6'
  libc = ctypes.cdll.LoadLibrary(GLIBC)
elif platform.system() == 'Darwin':
  LIBC = 'libc.dylib'
  libc = ctypes.cdll.LoadLibrary(LIBC)
else:
  LIBC = 'libc.so.6'
  libc = ctypes.cdll.LoadLibrary(LIBC)

LOBDS = './libOBDS.so'
try:
  lOBDS = ctypes.cdll.LoadLibrary(LOBDS)
except OSError as e:
  errmsg = 'compile the source code via: make && make clean'
  raise Exception(errmsg) from e

def c_associated(c_ptr):

  return bool(c_ptr)

class Sphere:

  # defines sizes for OBDS types found in headers
  numel = NUMEL = 256
  size_c_sphere_t = 32
  size_c_OBDS_Sphere_t = 256
  size_c_prop_t = 8
  size_c_random_t = 16
  size_c_generator_t = 32
  size_c_double = 8
  size_c_uint64_t = 8
  numel_list = (numel * log2(numel))
  SIZE = (size_c_sphere_t +
          size_c_OBDS_Sphere_t +
          25 * numel * size_c_prop_t +
          numel_list * size_c_prop_t +
          size_c_random_t +
          size_c_generator_t +
          size_c_double +
          size_c_uint64_t)
  SIZE = int(SIZE)

  def __init__ (this):

    assert(ctypes.sizeof(c_generator_t) == Sphere.size_c_generator_t)
    assert(ctypes.sizeof(c_random_t) == Sphere.size_c_random_t)
    assert(ctypes.sizeof(c_OBDS_Sphere_t) == Sphere.size_c_OBDS_Sphere_t)
    assert(ctypes.sizeof(c_sphere_t) == Sphere.size_c_sphere_t)
    assert(ctypes.sizeof(c_prop_t) == Sphere.size_c_prop_t)

    os.makedirs('run/bds/data/params', mode=0o777, exist_ok=True)
    os.makedirs('run/bds/data/positions', mode=0o777, exist_ok=True)
    os.makedirs('run/render/frames', mode=0o777, exist_ok=True)

    # allocates the workspace
    libc.malloc.restype = ctypes.c_void_p
    this.c_ptr_workspace = libc.malloc(ctypes.c_size_t(Sphere.SIZE))

    if not c_associated(this.c_ptr_workspace):
      errmsg = 'C MALLOC ERROR'
      raise RuntimeError(errmsg)

    # initializes the OBDS workspace
    lOBDS.particles_sphere_initializer.restype = ctypes.POINTER(c_sphere_t)
    this.c_ptr_spheres = lOBDS.particles_sphere_initializer(
      ctypes.c_void_p(this.c_ptr_workspace), 0
    )

    if not c_associated(this.c_ptr_spheres):
      libc.free(ctypes.c_void_p(this.c_ptr_workspace))
      errmsg = 'C OBDS Sphere Initializer ERROR'
      raise RuntimeError(errmsg)

    if this.c_ptr_spheres[0].log(this.c_ptr_spheres, sys.maxsize) != 0:
      libc.free(ctypes.c_void_p(this.c_ptr_workspace))
      errmsg = 'IO ERROR'
      raise RuntimeError(errmsg)

    return

  def __del__ (this):

    associated = bool(this.c_ptr_workspace)
    if not associated:
      libc.free(ctypes.c_void_p(this.c_ptr_workspace))

    return

  def update (this):
    this.c_ptr_spheres[0].update(this.c_ptr_spheres)
    return

  def log (this, step):
    this.c_ptr_spheres[0].log(this.c_ptr_spheres, step)
    return
