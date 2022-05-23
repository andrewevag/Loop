#!/bin/python3

import subprocess
from os import listdir
from os.path import isfile, join
import sys

program = []
if len(sys.argv) == 1:
	print(f'Since no argument is applied defaulding to executing ./programs/input.loop')
	program = "./programs/input.swhile"
else:
	program = sys.argv[1]

programs = "./programs"
onlyfiles = [join(programs, f) for f in listdir(programs) if isfile(join(programs, f))]
onlylibfiles = [f for f in onlyfiles if "input" not in f and "ifthenelse" not in f]
cmd = f'./swhile {program} --lib {" ".join(onlylibfiles)}'
print(cmd)
subprocess.run(cmd.split(' '))


