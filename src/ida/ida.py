# Usage:
# export IDA_PATH=/path/to/ida-6.9/idaq
# export BINSEC_PATH=/path/to/binsec
# $IDA_PATH -B "-S$BINSEC_PATH/src/ida/ida.py --output-dir=/output/path --call-graph=True --ida-graph=True /path/to/binary

import sys
import idautils
import idaapi
import idc
import argparse

def hex_wo_L(addr):
    return hex(addr)[:-1]

class BinaryScanner(object):
    def __init__(self):
        self._callbacks = []

    def add_callback(self, cb):
        self._callbacks += [cb]

    def _invoke_cbs(self, start, end, size, call_targets, successors, fname):
        for cb in self._callbacks:
            cb.process_bb(start, end, size, call_targets, successors, fname)

    def _invoke_cbs_fcn(self, start, name):
        for cb in self._callbacks:
            cb.process_function(start, name)

    def _get_call_target(self, instructions):
        ct = []
        for (instruction, ret) in instructions:
            for xref in idautils.XrefsFrom(instruction, 0):
                if xref.type == 17: # code_Near_Call
                    callsite_function_name = idc.GetFunctionName(xref.to)
                    # TODO: handle indirect calls ?
                    if callsite_function_name is not None and len(callsite_function_name) > 0:
                        ct.append((instruction, xref.to, ret))

        return ct

    def _scan_block(self, block, fname):
        current_bb_start = block.startEA
        instructions = idautils.Heads(block.startEA, block.endEA)
        last_instruction = None
        last_instruction_size = None
        call_instructions = []
        next_instruction = None

        for i in instructions:
            last_instruction = i
            last_instruction_size = idc.ItemSize(i)
            mnem = idc.GetMnem(i)
            is_call = mnem == "call"
            is_int = mnem == "int"

            if (is_call or is_int) and i != block.endEA:
                if not ida_graph:
                    next_instruction = i + last_instruction_size
                    bb_size = next_instruction - current_bb_start
                    ct = []
                    if is_call:
                        call_instructions = [(i, next_instruction)]
                        ct = self._get_call_target(call_instructions)
                    self._invoke_cbs(current_bb_start, i, bb_size, ct, [next_instruction], fname)

                    # start a new translation block
                    current_bb_start = idc.NextHead(i, block.endEA + 1)
                else:
                    next_instruction = i + last_instruction_size
                    call_instructions.append((i, next_instruction))

        if current_bb_start == idc.BADADDR:
            current_inst = last_instruction + last_instruction_size
            self._invoke_cbs(current_inst, idc.BADADDR, 0, [], [], fname)
            return

        if current_bb_start < block.endEA:
            bb_size = block.endEA - current_bb_start
            ct = []
            succs = []
            for succ_block in block.succs():
                succs.append(succ_block.startEA)

            if not ida_graph and mnem == "call":
                call_instructions = [(last_instruction, succs[0])]
            ct = self._get_call_target(call_instructions)

            if not ida_graph:
                self._invoke_cbs(current_bb_start, last_instruction, bb_size, ct, succs, fname)
            else:
                self._invoke_cbs(current_bb_start, i, bb_size, ct, succs, fname)

    def scan(self):
        for fcn in idautils.Functions():
            # only interested in functions in .text or .plt segment
            if not (idc.SegName(fcn) in ['extern']):
                fname = idc.GetFunctionName(fcn)
                f = idaapi.FlowChart(idaapi.get_func(fcn))
                # print out to ida file
                self._invoke_cbs_fcn(fcn, fname)
                for block in f:
                    self._scan_block(block, fname)


class Printer():
    def __init__(self):
        self._to_disassemble = []

    def process_function(self, start, name):
        # function: {start;name}
        print '-------------------'
        print('{' + ";".join([hex_wo_L(start), name]) + '}')
        pass

    def process_bb(self, start, end, size, call_targets, successors, fname):
        if size == 0:
            global to_disassemble
            self._to_disassemble += [start]
            return

        h = idautils.Heads(start, end+1)
        # basic block: [start;end;(instructions);(successors);(caller-callee-return)]
        print '[' + ";".join([hex_wo_L(start),
            '(' + ','.join(hex_wo_L(i) for i in h) + ')',
            '(' + ','.join(hex_wo_L(s) for s in successors) + ')',
            '(' + ','.join(hex_wo_L(c[0]) + "-" + hex_wo_L(c[1]) + "-" + hex_wo_L(c[2]) for c in call_targets) + ')'
            ]) + ']'

        # instruction: (addr;mnemonic;opcode;bb_addr;fname)
        for i in idautils.Heads(start, end+1):
            opcodes = []
            for op in GetManyBytes(i, ItemSize(i)):
                if len(str(hex(ord(op)))) % 2 != 0:
                    opcodes.append('0' + '%X' % ord(op))
                else:
                    opcodes.append('%X' % ord(op))
            print '(' + ";".join([hex_wo_L(i), GetDisasm(i).split(';', 1)[0].rstrip(),
                ' '.join(opcodes), hex_wo_L(start), fname]) + ')'
        print ""


class FuncsPrinter():
    def __init__(self, filename):
        self.fp = open(filename, "w")

    def process_function(self, start, name):
        # function: start,name
        print >>self.fp, ",".join([hex_wo_L(start), name])

    def process_bb(self, start, end, size, call_targets, successors, fname):
        pass


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--output-dir', metavar='output_dir', type=str,
                        default='.',
                        help='Output directory')
    parser.add_argument('-cg', '--call-graph', metavar='call_graph', type=bool,
                        default=False,
                        help='Generate call graph after disassembling')
    parser.add_argument('-i', '--ida-graph', metavar='ida_graph', type=bool,
                        default=False,
                        help='Original IDA graph (one basic block can contain multiple call instructions)')

    args = parser.parse_args(args=idc.ARGV[1:])
    output_dir = args.output_dir
    ida_graph = args.ida_graph

    if not os.path.isdir(output_dir):
        print output_dir, "is not a directory"
        idc.Exit(0)

    basename = os.path.join(output_dir, idc.GetInputFile())
    ida_file = os.path.join(output_dir, basename + ".ida")
    sys.stdout = sys.stderr = open(ida_file, "w")

    if args.call_graph:
        cg_file = os.path.join(output_dir, "callgraph.gdl")
        idc.GenCallGdl(cg_file, 'Call Gdl', idc.CHART_GEN_GDL)

    # waits for IDA's auto-analysis to finish before continuing IDC code execution
    idc.Wait()

    printer = Printer()
    funcs_printer = FuncsPrinter(os.path.join(output_dir, basename + ".funcs"))

    scanner = BinaryScanner()
    scanner.add_callback(printer)
    scanner.add_callback(funcs_printer)
    scanner.scan()

    # exit IDA
    idc.Exit(0)
