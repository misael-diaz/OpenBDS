#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             September 22, 2023

source: OpenBDS.py
author: @misael-diaz

Synopsis:
Uses ctypes to execute OBDS from Python.

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

from Sphere import Sphere

# instantiates OBDS Sphere object
spheres = Sphere()

# executes OBDS loop
for i in range(0, 16):
  spheres.update()
# logs the final sphere positions
spheres.log(16)
