#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             September 22, 2023

source: PRNG.py
author: @misael-diaz

Synopsis:
Defines the Pseudo Random Number Generator PRNG types in inc/util/random/type.h.

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

import ctypes

class c_generator_t(ctypes.Structure):
  pass

c_generator_t._fields_ = [
  ("count", ctypes.POINTER(ctypes.c_double)),
  ("state", ctypes.POINTER(ctypes.c_uint64)),
  ("seed",  ctypes.CFUNCTYPE(ctypes.c_int,    ctypes.POINTER(c_generator_t))),
  ("fetch", ctypes.CFUNCTYPE(ctypes.c_double, ctypes.POINTER(c_generator_t)))
]

class c_random_t(ctypes.Structure):
  pass

c_random_t._fields_ = [
  ("generator", ctypes.POINTER(c_generator_t)),
  ("fetch", ctypes.CFUNCTYPE(ctypes.c_double, ctypes.POINTER(c_random_t)))
]
