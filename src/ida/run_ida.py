#!/usr/bin/env python

# Usage:
# export IDA_PATH=/path/to/ida-6.9/idaq
# export BINSEC_PATH=/path/to/binsec
# export GRAPH_EASY_PATH=/path/to/graph-easy
# $BINSEC_PATH/src/ida/run_ida.py -i /path/to/binary --ida_graph --simple

import re
import sys
import argparse
import os
import shutil
from shutil import copyfile
import glob

# customized configuration paths
ida_path = os.environ['IDA_PATH']
binsec_path = os.environ['BINSEC_PATH']
graph_easy_path = os.environ['GRAPH_EASY_PATH']
binsec_bin_path = binsec_path + "/src/binsec"
ida_script_path = binsec_path + "/src/ida/ida.py"

# default arguments
error_file = "/tmp/error.log"
log_mode = "result" # "debug"

# remove dump files of IDA
def fix_ida():
    for file in os.listdir("/tmp/ida/"):
        file_name, file_ext = os.path.splitext(file)
        if file_ext == '.dmp':
            os.remove("/tmp/ida/" + file)

def main(bin_file, ida_graph, simple):
    fix_ida()
    print "Processing binary: " + bin_file
    bin_dir, bin_name = os.path.split(bin_file)
    out_dir = os.path.join(bin_dir, bin_name + "_ida")
    # create an output dir
    if os.path.exists(out_dir):
        shutil.rmtree(out_dir)
    os.makedirs(out_dir)
    # copy binary to output dir
    bin_tmp_file = out_dir + "/" + bin_name
    copyfile(bin_file, bin_tmp_file)
    os.chdir(out_dir)
    # run python script to get *.idb and *.ida
    ida_cmd = ida_path + " -B \"-S" + ida_script_path + " -o=" + out_dir + "\" " + bin_tmp_file
    os.system(ida_cmd)
    bin_idb_file = bin_tmp_file + ".idb"
    bin_ida_file = bin_tmp_file + ".ida"
    if ida_graph:
        parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + " -cg=True -i=True\" " + bin_idb_file
    else:
        parse_cmd = ida_path + " -A \"-S" + ida_script_path + " -o=" + out_dir + " -cg=True\" " + bin_idb_file
    os.system(parse_cmd)
    # convert call graph to dot format using graph-easy
    cg_cmd = graph_easy_path + " --input=callgraph.gdl --output=callgraph.dot 2> " + error_file
    os.system(cg_cmd)
    # run binida
    if simple:
        binida_cmd = binsec_bin_path + " -ida -isa x86 -quiet -ida-cfg-dot -ida-o-ida " + bin_ida_file + " -ida-loglevel " + log_mode
    else:
        binida_cmd = binsec_bin_path + " -ida -isa x86 -quiet -ida-cfg-dot -ida-no-simple -ida-o-ida " + bin_ida_file + " -ida-loglevel " + log_mode
    os.system(binida_cmd)

# Parse the input arguments
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-i','--bin_file',type=str,required=True,help="Full path of binary file")
    parser.add_argument('-o','--ida_graph',action='store_true',help="Original IDA graph (one basic block can contain multiple call instructions)")
    parser.add_argument('-s','--simple',action='store_true',help="Generate simple graph whose each node is the first instruction of basic block")
    args = parser.parse_args()

    main(args.bin_file, args.ida_graph, args.simple)
