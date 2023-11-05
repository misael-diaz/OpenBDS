#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
OpenBDS                                             November 04, 2023

source: slurm-submit.py
author: @misael-diaz

Synopsis:
Submits the OBDS App to the SLURM job scheduler running in the HPC cluster.

Copyright (c) 2023 Misael Diaz-Maldonado
This file is released under the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

References:
[0] R Johansson, Numerical Python: Scientific Computing and Data
    Science Applications with NumPy, SciPy, and Matplotlib, 2nd edition
[1] https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
[2] https://docs.python.org/3/library/typing.html
[3] https://docs.python.org/3/library/shutil.html
[4] https://docs.python.org/3/library/os.html
[5] https://slurm.schedmd.com/overview.html
"""

# imports facilities:
from os import chdir
from os import getcwd
from os import system
from os import makedirs
from shutil import copyfile
from shutil import copymode
from typing import List

# defines globals:
NUM_JOBS = 256

# defines methods:
def iota(n: int) -> List[int]:
  '''
  Synopsis:
  C++ like std::iota method.
  Returs the list of integers in the asymmetric range [0, `n`].
  '''
  return [i for i in range(n)]


def jobdir(idx: int) -> str:
  '''
  Synopsis:
  Returns the job directory (also referred to as the working directory).
  '''
  jobdir = f'{getcwd()}' + f'/job/' + f'{idx}'
  return jobdir


def strcat(str_1: str, str_2: str) -> str:
  '''
  Synopsis:
  Concatenates or joins strings.
  '''
  string = str_1 + str_2
  return string


def outdir(jobdir: str, reldir: str) -> None:
  '''
  Synopsis:
  Forwards its task to the string-concatenation method.
  '''
  return strcat(jobdir, reldir)


def submit(workdir: str) -> str:
  '''
  Synopsis:
  Generates the bash shell script for submitting the OBDS App to the
  HPC cluster that uses SLURM for job scheduling.
  Returns the script as a multiline f-string.
  '''

  status = strcat(workdir, '/run/bds/status/status.txt')

  script = (
    f'#!/bin/bash\n'                        # BASH shell script
    f'#source: submit.sh\n'                 # name of the shell script for job submission
    f'#SBATCH --partition=batch\n'          # serial job
    f'#SBATCH --nodes=1\n'                  # serial job
    f'#SBATCH --tasks-per-node=1\n'         # serial job
    f'#SBATCH --mem-per-cpu=1024\n'         # memory allocation may change in the future
    f'#SBATCH --time=167:45:00\n'           # job walltime
    f'#SBATCH --job-name=OpenBDS\n'         # job name
    f'#SBATCH --workdir="{workdir}"\n'      # full path to working directory
    f'#SBATCH --error=output-%J.err\n'      # logs error messages
    f'#SBATCH --output=output-%J.log\n'     # logs ordinary console output
    f'module load comp/gcc/13.2.0\n'        # imports GNU FORTRAN compiler, libs, etc.
    f'echo "unknown" > {status}\n'          # guards against job-scheduling hell on error
    f'./OpenBDS.bin\n'                      # executes the OBDS App
    f'stat=$(cat {status})\n'               # gets the status dumped by the OBDS App
    f'if [ "$stat" == "pending" ]; then\n'  # implements job auto-submission
    f'  echo "submitting job ..." && sbatch submit.sh\n'
    f'fi\n'
    f'exit 0\n'
  )

  return script


def gen_path(jobID: int, relpath: str) -> str:
  '''
  Synopsis:
  Generates full path to resource passed as relative path to the job directory.
  '''
  jdir = jobdir(jobID)
  path = outdir(jdir, relpath)
  return path


def make_outdir(path: str) -> None:
  '''
  Synopsis:
  Creates the output directory that the OBDS requires for doing IO. We have been careful
  not to make it world readable.
  '''
  makedirs(name = path, mode = 0o750, exist_ok = True)
  return


def dump_script(path: str) -> None:
  '''
  Synopsis:
  Dumps the shell script (for submitting the OBDS App) to the working directory.
  '''
  name = strcat(path, '/submit.sh')
  script = submit(path)
  with open(name, 'w') as f:
    f.write(script)
  return


def dump_scripts() -> None:
  '''
  Synopsis:
  Dumps the shell script `submit.sh` to the working directories for submitting the OBDS
  App to the SLURM job scheduler of the HPC cluster.
  '''
  jobs = iota(NUM_JOBS)
  for job in jobs:
    path = jobdir(job)
    dump_script(path)
  return


def copy_app(path: str) -> None:
  '''
  Synopsis:
  Copies the OBDS App from the current directory to the working directory.
  '''
  copyfile('OpenBDS.bin', strcat(path, '/OpenBDS.bin'))
  copymode('OpenBDS.bin', strcat(path, '/OpenBDS.bin'))
  return


def copy_apps() -> None:
  '''
  Synopsis:
  Copies the OBDS app to the working directories.
  '''
  jobs = iota(NUM_JOBS)
  for job in jobs:
    path = jobdir(job)
    copy_app(path)
  return


def make_outdirs() -> None:
  '''
  Synopsis:
  Makes the directories that the OBDS App uses to perform Input-Output IO operations.
  '''

  status = '/run/bds/status/'
  particles = '/run/bds/data/particles/'
  state = '/run/bds/state/'

  reldirs = relative_directories = [status, particles, state]

  jobs = iota(NUM_JOBS)
  for rel in reldirs:
    for job in jobs:
      path = gen_path(job, rel)
      make_outdir(path)

  return


def slurm_submits():
  old = getcwd()
  jobs = iota(NUM_JOBS)
  for job in jobs:
    path = jobdir(job)
    chdir(path)
    system('sbatch submit.sh')
    chdir(old)
  return

# driver code:
make_outdirs()  # makes the output directories required by the OBDS App
dump_scripts()  # dumps the shell scripts for submitting the job to SLURM
copy_apps()     # copies the OBDS app to the work directories
slurm_submits() # submits the OBDS apps to the SLURM scheduler

'''
COMMENTS:
We use this script to ease the task of submitting several replicas of the same simulation
to the HPC cluster.
'''
