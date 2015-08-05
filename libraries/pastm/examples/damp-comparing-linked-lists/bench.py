#!/usr/bin/env python

import argparse, subprocess, pdb, re

parser = argparse.ArgumentParser()

parser.add_argument("-iters", type=int, help="Number of iterations for each STM implementation", default=3)
parser.add_argument("-opt", type=str, help="Optimization level", default="")
parser.add_argument("-cores", type=int, help="Number of cores to go up to", default=4)
parser.add_argument("-stm", type=str, help="Which STM to use", default="partial")

args = parser.parse_args()

makeCmds = [('mvar', 'mvar') , ('partial straight', 'straightForwardParital'), ('partial dissected', 'dissectedPartial'), ('cas', 'cas')]

filename = 'Times.txt'

l = '               '

for i in range(args.cores):
    for j in range(args.iters):
        l = l + 'C' + str(i) + '-' + str(j) + ' '
        
subprocess.Popen('echo \"' + l + '\" > ' + filename, shell = True).wait()      
    
for cmd, prog in makeCmds:
    subprocess.Popen('make ' + cmd + ' OPT=\"' + args.opt + '\"', shell = True).wait()
    l = cmd + ' ' 
    for i in range(args.cores):
        for j in range(args.iters):
            print('./' + prog + args.opt + ' +RTS -N' + str(i+1) + '(iteration ' + str(j) + ')')
            x = subprocess.check_output('./' + prog + args.opt + ' +RTS -N' + str(i+1), shell=True)
            time = re.search('Time = .*', str(x))
            time = re.search('[0-9]*\.[0-9]*', time.group(0))
            l = l + time.group(0) + ' ' 
    subprocess.Popen('echo \"' + l + '\" >> ' + filename, shell = True).wait()  
        
