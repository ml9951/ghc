#!/usr/bin/env python
import sys, getopt, re, os, pdb, argparse
import statistics as st
import functools

#use pdb.set_trace() to set a breakpoint

parser = argparse.ArgumentParser()
parser.add_argument("-out", type=str, help="dump latex tables to specified file", default=None)
parser.add_argument("-file", type=str, help="file to be processed")
parser.add_argument("-figuresonly", action='store_true')
args = parser.parse_args()

outStream = sys.stdout if args.out is None else open(args.out, 'w')

baseline = "CPSFull"

def toValue(v):
    try:
        return int(v)
    except ValueError:
        try:
            return float(v)
        except ValueError:
            return v

Keys = set()
def processChunk(chunk):
    global iters
    global Keys
    lines = chunk.split('\n')
    chunkDict = {}
    for l in lines:
        keyVal = [x.strip() for x in l.split('=')]
        if len(keyVal) == 2:
            key = keyVal[0]
            chunkDict.setdefault(key, []).append(toValue(keyVal[1]))
            Keys.add(key)
    return chunkDict

def mean(l):
    sum = 0
    for x in l:
        sum = sum + x
    return sum / len(l)

def dumpRow(data, k, base):
    s = k
    s2 = k + '$\\Delta$'
    isCommon = True
    for stm, stmData in data:
        if k in stmData:
            s += ' & ' + ("%.2f" % mean(stmData[k]))
            change = 0 if base == 0 else ((base - mean(stmData[k])) / base) * -100
            s2 += ' & ' + ("%.2f" % change) + '\%'
        else:
            s += ' & - ' 
            isCommon = False
    outStream.write(s + '\\\\\\hline\n')
    if isCommon:
        outStream.write(s2 + '\\\\\\hline\n')

def dumpTex(data, benchName, date):
    global Keys
    iters = None
    keys = sorted(list(Keys))
    outStream.write("\\begin{figure}[H]\n")
    outStream.write("\\noindent\\adjustbox{max width=\\textwidth}{%\n")
    outStream.write("\\centering\n")
    outStream.write('\\begin{tabular}{|' + functools.reduce (lambda x, y: ' c |' + x, range(len(data)+1), '') + '}\n')
    outStream.write("\\hline\n")
    orderedData = sorted(data.items())
    stms = [a for a, b in orderedData]
    outStream.write (functools.reduce (lambda x, y: x + ' & ' + y, stms, '') + '\\\\\\hline\n')
    for key in keys:
        base = (mean(data[baseline][key])) if key in data[baseline] else 1
        iters = len(data[baseline][key]) if key in data[baseline] else iters
        dumpRow(orderedData, key, base)
    outStream.write("\\end{tabular}\n")
    outStream.write("}\n")
    outStream.write ("\\caption{" + benchName + " date: " + date + " Average of " + str(iters) + " iterations}\n")
    outStream.write("\\end{figure}\n")

def doBenchmark(benchName, text, date):
    data = {}
    stmChunks = re.split('stm = .*', text)
    stmChunks = [chunk for chunk in stmChunks if chunk.rstrip()]
    stms = re.findall('stm = .*', text)
    for stm, chunk in zip(stms, stmChunks):
        chunkDict = processChunk(chunk)
        whichSTM= [x.strip() for x in stm.split('=')]
        data[whichSTM[1]] = chunkDict
    dumpTex(data, benchName, date)

def main():
    origDir = os.path.dirname(os.path.realpath(__file__))
    if not(args.figuresonly):
        outStream.write('\\documentclass[12pt]{article}\n\\usepackage[margin=0.5in]{geometry}\n\\usepackage{adjustbox,lipsum}\n\\usepackage{float}\n\\begin{document}\n')
    os.chdir(origDir)
    doBenchmark("Linked List", open(args.file).read(), "")
    if not(args.figuresonly):
        outStream.write('\\end{document}\n')

if __name__ == "__main__":
    main()    
