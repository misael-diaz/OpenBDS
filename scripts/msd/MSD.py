#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             September 18, 2023

source: MSD.py
author: @misael-diaz

Synopsis:
Obtains the time-averaged Mean Squared Displacement MSD.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] R Johansson, Numerical Python: Scientific Computing and Data
    Science Applications with NumPy, SciPy, and Matplotlib, 2nd edition
"""

from numpy import zeros
from numpy import arange
from numpy import vstack
from numpy import newaxis
from numpy import loadtxt
from numpy import savetxt

def get_params_OBDS():
  '''
  Synopsis:
  Reads the OBDS parameters (stored in a plain text file) into a dictionary.
  Note that the parameters in the file are stored in a dictionary fashion for convenience.
  '''
  params = {}
  fname = 'run/bds/params/params-bds.txt'
  with open(fname, 'r') as f:

    for line in f:

      key, val = line.split(':')
      if key != 'SHA512SUM':
        param = float(val)
      else:
        param, newline = val.split('\n')

      params[key] = param

  return params

# gets OBDS parameters from the configuration file
params_OBDS = get_params_OBDS()
time_step_OBDS = params_OBDS['TIME_STEP']
num_spheres_OBDS = int(params_OBDS['NUM_SPHERES'])

# defines hardcoded parameters in the OBDS code
num_steps_OBDS = int(256 / time_step_OBDS)
# defines hardcoded time interval that the OBDS app uses to write particle data
wr_time_step_OBDS = 2**-9
wr_step_size_OBDS = int(wr_time_step_OBDS / time_step_OBDS)

# number of steps that have been written for post-processing (accounts for the final time)
num_wr_steps_OBDS = 1 + int(num_steps_OBDS / wr_step_size_OBDS)
# array of times that correspond to the times at which data has been written
times = arange(0, num_steps_OBDS + wr_step_size_OBDS, wr_step_size_OBDS) * time_step_OBDS
times = times[newaxis, :]

# computes the (rotational) time-averaged MSD:

first_step_size, final_step_size = (1, num_wr_steps_OBDS + 1)
MSD = zeros([1, (final_step_size - first_step_size)])
# loop-invariant: so far we have obtained `n + 1' elements of the MSD array (MSD[0] = 0)
for n, step in enumerate( range(first_step_size, final_step_size - 1) ):
  begin, end = [0, (num_wr_steps_OBDS - step)]
  num_steps = (end - begin)
  displacements = zeros([num_spheres_OBDS])
  # loop-invariant: so far we have considered `i' steps to obtain the current MSD element
  for i in range(begin, end):

    # gets the IDs of the respective data files
    step_id = i * wr_step_size_OBDS
    step_id_next = (i + step) * wr_step_size_OBDS

    # fetches the data in the files
    fdata = f'run/bds/data/positions/spheres-{step_id}.txt'
    fdata_next = f'run/bds/data/positions/spheres-{step_id_next}.txt'
    data = loadtxt(fdata).transpose()
    data_next = loadtxt(fdata_next).transpose()

    # gets the Euler angles at times `t' and `t + dt', respectively
    x1, y1, z1 = data[6:9, :]
    x2, y2, z2 = data_next[6:9, :]

    # obtains the squared displacements in the time interval [`t', `t + dt']
    displacements += ( (x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2 )

  # averages with respect to the total number of displacements considered
  avg_displacement = displacements.sum() / (3 * num_spheres_OBDS * num_steps)

  # computes the `n + 1' element of the MSD array corresponding to a time `times[n + 1]'
  MSD[0, n + 1] = avg_displacement

# saves the (rotational) MSD results to a plain text file
results = vstack([times, MSD]).transpose()
savetxt('results-rotational-MSD.txt', results)
