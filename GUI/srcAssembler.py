#!/bin/python3

import os
from posix import listdir
from posixpath import join
cwd = os.getcwd()
cwd = cwd+'/src'
cpp_files = [f for f in listdir(cwd) if os.path.isfile(
    join(cwd, f)) and os.path.splitext(f)[1] == '.cpp']
for f in cpp_files:
    print('./src/'+f)

