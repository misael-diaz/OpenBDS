#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             July 20, 2023

source: genpovs.py
author: @misael-diaz

Synopsis:
Generates a povray file for each simulation step produced by the equilibration run.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] R Johansson, Numerical Python: Scientific Computing and Data
    Science Applications with NumPy, SciPy, and Matplotlib, 2nd edition
[1] https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
[2] https://docs.python.org/3/library/stdtypes.html#str.split
[3] https://docs.python.org/3/library/glob.html
"""

from glob import glob
from numpy import loadtxt

def getBDSParams():
  '''
  Synopsis:
  Reads the BDS parameters (stored in a plain text file) into a dictionary.
  Note that the parameters in the file are stored in a dictionary fashion for convenience.
  '''
  params = {}
  fname = 'params-bds.txt'
  with open(fname, 'r') as f:

    for line in f:

      key, val = line.split(':')
      if key != 'SHA512SUM':
        param = float(val)
      else:
        param, newline = val.split('\n')

      params[key] = param

  return params

# defines the header for the povray files (same for each)
params = getBDSParams()
lim = params['LIMIT']
t = edge_thickness = 0.0625
header = (
    f'#version 3.7;\n'
    f'#include "colors.inc"\n'
    f'global_settings {{ assumed_gamma 1.0 }}\n'
    f'global_settings {{ max_trace_level 256 }}\n'
    f'global_settings {{ ambient_light color White }}\n'
    f'global_settings {{ ambient_light White }}\n'
    f'background {{ color Black }}\n'
    f'light_source {{ <128, 128, 128> color 1.8 * White }}\n'
    f'camera {{\n'
    f'sky <0, 0, 1>\n'
    f'angle 50\n'
    f'right <-4/3, 0, 0>\n'
    f'location <{4 * lim}, 0, {2 * lim}>\n'
    f'look_at <0, 0, 0>\n'
    f'}}\n'
    f'// system box edges\n'
    f'cylinder {{<{-lim},{-lim},{-lim}>, <{+lim},{-lim},{-lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{+lim},{-lim}>, <{+lim},{+lim},{-lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{-lim},{+lim}>, <{+lim},{-lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{+lim},{+lim}>, <{+lim},{+lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{-lim},{-lim}>, <{-lim},{+lim},{-lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{+lim},{-lim},{-lim}>, <{+lim},{+lim},{-lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{-lim},{+lim}>, <{-lim},{+lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{+lim},{-lim},{+lim}>, <{+lim},{+lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{-lim},{-lim}>, <{-lim},{-lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{+lim},{-lim},{-lim}>, <{+lim},{-lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{-lim},{+lim},{-lim}>, <{-lim},{+lim},{+lim}> {t} pigment {{White}}}}\n'
    f'cylinder {{<{+lim},{+lim},{-lim}>, <{+lim},{+lim},{+lim}> {t} pigment {{White}}}}\n'
)

run = 'run/equilibration/data/positions/'
out = 'run/equilibration/render/frames/'
# generates a povray file for each datafile (storing the particle positions) nested in run
for datafile in glob('run/equilibration/data/positions/*.txt'):

  fname, ext = datafile.split('.')
  _, fname = fname.split(run)
  povfile = f'{out}{fname}.pov'
  with open(povfile, 'w') as f:

    f.write(header)

    positions = loadtxt(datafile)
    for position in positions:

      x, y, z = position
      sphere = (
        f'sphere {{ < {x}, {y}, {z} > 1 ' +
        f'texture {{ pigment {{ color Gray transmit 0 }} }} }}\n'
      )

      f.write(sphere)

"""
COMMENTS:
After running this code we can use povray to produce the frames and then ffmpeg to create
a video of the equilibration run.
"""
