#!/bin/bash
#
# OpenBDS					August 14, 2023
#
# source: render.sh
# author: @misael-diaz
#
# Synopsis:
# Uses povray to render the povray files *.pov in the `run/bds/render/frames/' directory.
#
# Copyright (c) 2023 Misael Diaz-Maldonado
# This file is released under the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# References:
# [0] man bash(1)

ls=$(which ls)
if [ ! -n "$ls" ] || [ ! -x "$ls" ]
then
  echo "render.sh: ls util not found in PATH: $PATH" >&2
  exit 1
fi

sed=$(which sed)
if [ ! -n "$sed" ] || [ ! -x "$sed" ]
then
  echo "render.sh: sed util not found in PATH: $PATH" >&2
  exit 1
fi

grep=$(which grep)
if [ ! -n "$grep" ] || [ ! -x "$grep" ]
then
  echo "render.sh: grep util not found in PATH: $PATH" >&2
  exit 1
fi

povray=$(which povray)
if [ ! -n "$povray" ] || [ ! -x "$povray" ]
then
  echo "render.sh: povray tool not found in PATH: $PATH" >&2
  exit 1
fi

version=$("$povray" --version 2>&1 | "$sed" -n 's/^POV-Ray \([0-9]\.[0-9]\).*/\1/p')
if [ ! "$version" == "3.7" ]
then
  echo "render.sh: script expects POV-Ray version 3.7" >&2
  exit 1
fi

frames="run/bds/render/frames/"
if [ ! -d "$frames" ]
then
  echo "render.sh: script expectes the directory $frames in $PWD" >&2
  exit 1
fi

cd "$frames"

"$ls" | "$grep" pov | while read povfile
do
  "$povray" "$povfile" 2>/dev/null
done

cd "$OLDPWD"

exit 0
