## Install
- IDA Pro version 6.9.151221 (32-bit) and python version 2.7
- Graph-Easy version 0.7.6 (https://metacpan.org/pod/Graph::Easy)
- BINSEC (see README)

## Workflow
The goal of the BINIDA plugin is to extract information of the input binary in
x86 using the disassembler IDA Pro, then construct the control flow graphs (CFG)
that is represented by the data structure of BINSEC's CFG. It provides an
additional option to disassemble x86 binaries apart from the existing module
`src/disasm`. The structure of the BINIDA source code is as follows: 

`src/ida`
- `ida.py`: IDAPython script for parsing the input binary, generating the call
  graph and outputting the ida file containing the binary information. 
- `run_ida.py`: Python script for executing the plugin BINIDA.
- `ida.ml(i)`: the main function.
- `ida_options.ml(i)`: identifying input arguments.
- `ida_cg.ml(i)`: parsing the callgraph.
- `ida_cfg.ml(i)`: parsing the ida file to generate CFGs.
- `ida_utils.ml(i)`: some utilities.

The ida file contain the information of functions, basic blocks and instructions as follows:
```
Function    {start_addr; func_name}
BasicBlock  [start_addr; (instructions); (bb_preds); (bb_succs); \
            (caller_call_addr-callee_start_addr-caller_return_addr)]
Instruction (addr; disasm; opcodes; bb_start_addr; func_name)
```

## Usage

#### 1. Runing IDAPython script `ida.py`
 - `--output-dir`: the absolute path of the output directory.
 - `--call-graph` (True/False): generate the call graph.
 - `--ida-graph` (True/False): if True, generate the original IDA graph, meaning
   one basic block could have multiple call instructions. Otherwise, we consider
   function calls as basic block boundaries. 

```
export IDA_PATH=/path/to/ida-6.9/idaq
export BINSEC_PATH=/path/to/binsec
$IDA_PATH -B "-S$BINSEC_PATH/src/ida/ida.py --output-dir=/output/path \
--call-graph=True --ida-graph=True" /path/to/binary
```

#### 2. Running `run_ida.py`
 - `--bin_file`: the absolute path of the x86 binary.
 - `--ida_graph`: similar to the option `--ida-graph` of `ida.py`.
 - `--simple`: nodes of CFG are basic blocks. If not, nodes are instructions.

```
export IDA_PATH=/path/to/ida-6.9/idaq
export BINSEC_PATH=/path/to/binsec
export GRAPH_EASY_PATH=/path/to/graph-easy
$BINSEC_PATH/src/ida/run_ida.py --bin_file /path/to/binary --ida_graph --simple
```
