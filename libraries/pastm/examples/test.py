#!/usr/bin/env python

import sys, getopt, re, os, signal
import subprocess, pdb
import argparse
from time import gmtime, strftime
import smtplib

parser = argparse.ArgumentParser()

parser.add_argument("-iters", type=int, help="Number of iterations for each STM implementation", default="50")
parser.add_argument("-prog", type=str, help="Program to be run")
args = parser.parse_args()

for i in range(args.iters):
    out = subprocess.check_output('./' + args.prog + ' +RTS -N4', shell=True)
    out = str(out)
    errors = re.findall('Error', out)
    passed = re.findall('Success', out)
    if len(errors) > 0 or len(passed) != 1:
        print('Iteration ' + str(i) + ' failed!\nOutput:\n')
        print(out)
    else:
        print('Iteration ' + str(i) + ' passed')
















