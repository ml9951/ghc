#!/usr/bin/env python


import sys, getopt, re, os, signal
import subprocess
import argparse
from time import gmtime, strftime
from email.mime.text import MIMEText
import smtplib

parser = argparse.ArgumentParser()

parser.add_argument("-iters", type=int, help="Number of iterations for each STM implementation", default="10")
args = parser.parse_args()

stms = ["Full", "Partial", "CPSFull"]

prog = "synthetic"

filename = prog + 'Times.txt'

subprocess.Popen('echo \"\" > ' + filename, shell = True).wait()

for stm in stms:
    subprocess.Popen('echo \"stm = ' + stm + '\" >> ' + filename, shell = True).wait()
    for i in range(args.iters):
        subprocess.Popen('./' + prog + stm + ' +RTS -N4 > currentTime.txt', shell = True).wait()
        subprocess.Popen('cat currentTime.txt; cat currentTime.txt >> ' + filename, shell = True).wait()
    subprocess.Popen('echo \"end\" >> ' + filename, shell = True).wait()








