#!/bin/bash
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem-per-cpu=1024
#SBATCH --time=167:45:00
#SBATCH --job-name=OpenBDS
#SBATCH --workdir="$WORK/api/clang/src/bds/python"
#SBATCH --error=output-%J.err
#SBATCH --output=output-%J.log
#
# OpenBDS						October 25, 2023
#
# source: submit.sh
# author: @misael-diaz
#
# Synopsis:
# Submits the OpenBDS OBDS code.
# Implements auto-scheduling.
#
# Copyright (c) 2023 Misael Diaz-Maldonado
# This file is released under the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#

# loads the (most recent) Python 3 module
module load python3/3.10.2

# dumps ``unknown'' to status file as a fail-safe mechanism
mkdir -p run/bds/status  && echo "unknown" > "run/bds/status/status.txt"

# executes the OBDS code
python3 OpenBDS.py

stat=`cat run/bds/status/status.txt`
if [ "$stat" == "reschedule" ]; then
    echo "rescheduling job..." && sbatch submit.sh
fi

exit 0

# COMMENTS:
# For the auto-scheduling to work the OBDS code must exit normally before the walltime.
# If the job gets cancelled or aborts due to some error this script will **not** submit
# the job since it assumes that the developer (or user) needs to check what went wrong.
# This means that the OBDS code must check the elapsed-time periodically (via the system
# clock or something like that) to dump the status ``reschedule'' before exiting normally.
# (If it does not do that, then it will get killed by the SLURM scheduler and this script
# will assume that something went wrong and that it should **not** submit the job.)
# If the OBDS code has really finished its task, then it can write whatever like ``done''
# or ``completed'' to the status file to switch off the auto-scheduling.
#
# It is assumed that the OBDS code dumps checkpoint files periodically so that the code
# is able to continue from the last checkpoint.
