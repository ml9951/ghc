#!/usr/bin/env python


import sys, getopt, re, os, signal
import subprocess
import argparse
from time import gmtime, strftime
from email.mime.text import MIMEText
import smtplib

parser = argparse.ArgumentParser()

parser.add_argument("-iters", type=int, help="Number of iterations for each STM implementation", default="10")
parser.add_argument("-prog", type=str, help="executable to run")
args = parser.parse_args()

filename = 'times.txt'

subprocess.Popen('echo \"\" > ' + filename, shell = True).wait()

subprocess.Popen('echo \"stm = ' + 'stm' + '\" >> ' + filename, shell = True).wait()
for i in range(args.iters):
    subprocess.Popen('./' + args.prog + ' -i 1000 +RTS -N4 -V0 > currentTime.txt', shell = True).wait()
    subprocess.Popen('cat currentTime.txt; cat currentTime.txt >> ' + filename, shell = True).wait()
subprocess.Popen('echo \"end\" >> ' + filename, shell = True).wait()








