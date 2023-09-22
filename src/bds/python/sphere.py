#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             September 22, 2023

source: sphere.py
author: @misael-diaz

Synopsis:
Defines the OBDS sphere types in inc/particles/sphere/types.h.

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
from PRNG import c_random_t

class c_prop_t(ctypes.Union):
  _fields_ = [("data", ctypes.c_double),
              ("bin", ctypes.c_uint64)]

class c_OBDS_Sphere_t(ctypes.Structure):
  _fields_ = [("x",       ctypes.POINTER(c_prop_t)),
              ("y",       ctypes.POINTER(c_prop_t)),
              ("z",       ctypes.POINTER(c_prop_t)),
              ("r_x",     ctypes.POINTER(c_prop_t)),
              ("r_y",     ctypes.POINTER(c_prop_t)),
              ("r_z",     ctypes.POINTER(c_prop_t)),
              ("_dx",     ctypes.POINTER(c_prop_t)),
              ("_dy",     ctypes.POINTER(c_prop_t)),
              ("_dz",     ctypes.POINTER(c_prop_t)),
              ("a_x",     ctypes.POINTER(c_prop_t)),
              ("a_y",     ctypes.POINTER(c_prop_t)),
              ("a_z",     ctypes.POINTER(c_prop_t)),
              ("d_x",     ctypes.POINTER(c_prop_t)),
              ("d_y",     ctypes.POINTER(c_prop_t)),
              ("d_z",     ctypes.POINTER(c_prop_t)),
              ("f_x",     ctypes.POINTER(c_prop_t)),
              ("f_y",     ctypes.POINTER(c_prop_t)),
              ("f_z",     ctypes.POINTER(c_prop_t)),
              ("t_x",     ctypes.POINTER(c_prop_t)),
              ("t_y",     ctypes.POINTER(c_prop_t)),
              ("t_z",     ctypes.POINTER(c_prop_t)),
              ("tmp",     ctypes.POINTER(c_prop_t)),
              ("temp",    ctypes.POINTER(c_prop_t)),
              ("bitmask", ctypes.POINTER(c_prop_t)),
              ("list",    ctypes.POINTER(c_prop_t)),
              ("id",      ctypes.POINTER(c_prop_t)),
              ("pad0",    ctypes.c_uint64, 64),
              ("pad1",    ctypes.c_uint64, 64),
              ("pad2",    ctypes.c_uint64, 64),
              ("pad3",    ctypes.c_uint64, 64),
              ("pad4",    ctypes.c_uint64, 64),
              ("pad5",    ctypes.c_uint64, 64)]

class c_sphere_t(ctypes.Structure):
    pass

c_sphere_t._fields_ = [
  ("props", ctypes.POINTER(c_OBDS_Sphere_t)),
  ("random", ctypes.POINTER(c_random_t)),
  ("update", ctypes.CFUNCTYPE(ctypes.c_int, ctypes.POINTER(c_sphere_t))),
  ("log",    ctypes.CFUNCTYPE(ctypes.c_int, ctypes.POINTER(c_sphere_t), ctypes.c_size_t))
]
