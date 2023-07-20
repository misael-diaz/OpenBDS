#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             July 20, 2023

source: msd.py
author: @misael-diaz

Synopsis:
Plots the time evolution of the Mean Squared Displacement MSD.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] R Johansson, Numerical Python: Scientific Computing and Data
    Science Applications with NumPy, SciPy, and Matplotlib, 2nd edition
"""

from numpy import loadtxt
import matplotlib as mpl
from matplotlib import pyplot as plt

t, msd = loadtxt('msd.txt').transpose()

plt.close('all')
plt.ion()
fig, ax = plt.subplots()
ax.loglog(t, 2 * t, linestyle='--', color='black', label='theory')
ax.loglog(t, msd, linestyle='-', color='red', label='bds')
ax.set_title('Mean Squared Displacement MSD of Brownian Spheres')
ax.set_xlabel('time')
ax.set_ylabel('MSD')
ax.legend()
