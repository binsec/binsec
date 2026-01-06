//TODO write a description for this script
//@author
//@category CustomerSubmission
//@keybinding
//@menupath
//@toolbar

import java.lang.StringBuilder;
import ghidra.app.script.GhidraScript;
import ghidra.program.model.util.*;
import ghidra.program.model.reloc.*;
import ghidra.program.model.data.*;
import ghidra.program.model.block.*;
import ghidra.program.model.symbol.*;
import ghidra.program.model.scalar.*;
import ghidra.program.model.mem.*;
import ghidra.program.model.listing.*;
import ghidra.program.model.lang.*;
import ghidra.program.model.pcode.*;
import ghidra.program.model.address.*;

public class ghidra_export extends GhidraScript {

    private static final String HEX = "0123456789abcdef";
    private static String hexlify(byte[] bytes) {
	char[] chars = new char[bytes.length * 2];
	for (int j = 0; j < bytes.length; j++) {
	    int v = bytes[j] & 0xFF;
	    chars[j * 2] = HEX.charAt(v >>> 4);
	    chars[j * 2 + 1] = HEX.charAt(v & 0x0f);
	}
	return new String(chars);
    }

    public void run() throws Exception {
	Listing listing = currentProgram.getListing();
	for (MemoryBlock b : currentProgram.getMemory().getBlocks()) {
	    if (b.isInitialized()) {
		AddressSet s = new AddressSet(b.getStart(), b.getEnd());
		for (Instruction i : listing.getInstructions(s, true)) {
		    println("(address . 0x" + i.getAddress() + ")");
		    println("(opcode . 0x" + hexlify(i.getBytes()) + ")");
		    println("(size . " + i.getBytes().length + ")");
		    println("(mnemonic . " + '"' + i + '"' + ")");
		    println("(kind . " + '"' + i.getFlowType() + '"' + ")");
		    StringBuilder k = new StringBuilder();
		    k.append("(successors .");
		    for (Address a : i.getFlows()) {
			if (!a.isExternalAddress()) {
			    k.append(" 0x");
			    k.append(a);
			}
		    }
		    k.append(")");
		    println(k.toString());
		}
	    }
	}
    }

}
