##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2016-2019                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# Lib.mk contains the definitions for the functions/macros called below.
include Lib.mk

$(eval $(call defcunit,base,\
	logger \
	base_logger \
	bigint string_utils \
	fstack \
	natural \
	hashamt \
	basic_types \
	bitvector \
	bitset \
	size \
	interval sequence \
	binstream \
	mnemonic \
	virtual_address \
	cli \
	list_utils print_utils file_utils array_utils \
	machine \
	worklist \
	utils \
	prettytbl,sigs,CEA))

# Loader
$(eval $(call defcunit,loader,\
	elf_options \
	loader_buf \
	loader_elf loader_dump loader_pe loader \
	loader_utils,\
	loader_types loader_sigs,CEA))

$(eval $(call defcunit,kernel,\
	config kernel_options \
	kernel_functions kernel_core,,CEA))

$(eval $(call defcunit,dba,\
	dba dba_printer dba_types dba_to_formula errors dba_utils dba_visitor,\
	,CEA))

$(eval $(call defcunit,utils,\
	colors \
	directive \
	cfg \
	dhunk \
	instruction \
	network_io \
	instParsing,,CEA))


##
# Debug Dwarf
##
$(eval $(call defcunit,dwarf,\
	dwarf_options dwarf_expr \
	dwarf_frame dwarf_lines dwarf_cunit \
	dwarf,,CEA))

##
# Protobuf
##
PROTO_DIR = proto
PROTO_SRC_FILES = \
	common instruction libcall syscall \
	dba trace analysis_config config message
PROTO_FILES = $(PROTO_SRC_FILES:%=$(PROTO_DIR)/%.proto)

BINSEC_DISTRIB_FILES += $(PROTO_FILES)
BINSEC_LICENSE_CEA += $(PROTO_FILES)

# License ?

# Set and created by configure PIQI_DIR = piqi
PIQI_SRC_FILES = $(PROTO_SRC_FILES)
PIQI_FILES = $(PIQI_SRC_FILES:%=$(PIQI_DIR)/%.piqi)
PIQI_ML_FILES = \
	$(PIQI_SRC_FILES:%=$(PIQI_DIR)/%_piqi.ml) \
	$(PIQI_SRC_FILES:%=$(PIQI_DIR)/%_piqi_ext.ml)
GENERATED_FILES += \
	$(PIQI_FILES) \
	$(PIQI_ML_FILES)

BINSEC_ML_FILES += $(PIQI_ML_FILES)
DIRS += $(PIQI_DIR) # PIQI_DIR is defined elsewhere


##
# Parsers
##
$(eval $(call defcunit_parser,parser,parse_utils,,infos parse_helpers,\
	dbacsl_parser parser parser_infos policy_parser SMTParserWp,\
	dbacsl_token lexer lexer_infos policy_token SMTLexerWp,CEA))

##
# SMT
##
$(eval $(call defcunit_parser,smtlib,\
	smtlib_options \
	smtlib_pp smt_model smtlib_utils,\
	smtlib,\
	locations,\
	smtlib_parser,smtlib_lexer,CEA))

##
# Formula
##

$(eval $(call defcunit,formula,\
	formula_options \
	formula formula_utils \
	formula_to_smtlib \
	smtlib_to_formula \
	formula_pp \
	prover \
	formula_transformation solver \
	formula_main,,CEA))

$(eval $(call defcunit,disasm/x86,\
	x86_options lreader x86Util x86pp \
	predba x86Instruction x86decoder x86toDba,\
	x86Types,CEA))

$(eval $(call defcunit,disasm/arm,arm_options armToDba,,CEA))

$(eval $(call defcunit,disasm/riscv,riscv_options riscv_arch riscv_to_dba,,CEA))

$(eval $(call defcunit,disasm/simplify,\
	simplification_options \
	simplification_dba_utils \
	simplification_dba_block \
	simplification_dba_prog \
	simplification_dba,,CEA))

$(eval $(call defcunit,llvm,\
	transfer_functions \
	generic_decoder_sig \
	generic_decoder llvm_decoder,,CEA))

$(eval $(call defcunit,static/types,\
	static_options simulate_options \
	smt_bitvectors region_bitvector,,CEA))

$(eval $(call defcunit,ast,instr_cfg cfgraph ast_builder,,CEA))

$(eval $(call defcunit,disasm,\
	disasm_options \
	decode_utils \
	disasm_core \
	disasm_cfg \
	disasm,,CEA))

$(eval $(call defcunit,ida,\
	ida_options \
	ida_utils \
	ida_cfg \
	ida_cg \
	ida,,CEA))

$(eval $(call defcunit,static,pmap static_types static_utils,,CEA))

$(eval $(call defcunit,static/simulation,\
	simulate_utils \
	concrete_state concrete_eval \
	simulate,,CEA))

$(eval $(call defcunit,static/interpreter,simulation concrete,,CEA))

$(eval $(call defcunit,static/ai/base,ai_options normalize_instructions,,CEA))

$(eval $(call defcunit,static/ai/domains,\
	domain_common domain_interval range kset union_find taint,,CEA))

$(eval $(call defcunit,static/ai,\
	display ai_utils \
	backward_analysis \
	normalize_predicate high_level_predicate \
	reduced_product nonrelational \
	ai_results \
	ai,ai_sigs,CEA))

$(eval $(call defcunit,sse,\
	sse_options \
	sse_symbolic sse_graph \
	sse_prune \
	sse_types sse_utils \
	sse_smt \
	sse,,CEA))

$(eval $(call defcunit,backwards,bw_options opaque bw_main,,CEA))

$(eval $(call defcunit,xtrasec,\
	xtrasec_options parsepin formula_decoder xtrasec,,CEA))


$(eval $(call defcunit,binpatcher,binpatcher_options binpatcher,,CEA))

$(eval $(call defcunit,dynamic/base,\
	trace_config trace_options trace_type \
	fdInput \
	dse_options \
	path_predicate_env \
	path_predicate_optim path_predicate_formula \
	path_predicate_utils,,CEA))

DYNAMIC_DIR = dynamic
DDSE_DIR = $(DYNAMIC_DIR)/dse

$(eval $(call defcunit,$(DDSE_DIR)/libcall_stubs,\
	libcall_utils \
	windows_stubs \
	call_convention libc_stubs libcall_stubs,libcall_types,CEA))

# DDSE_LIBCALL_SRC_FILES_CEA_IMAG = libc_stubs libcall_stubs
# DDSE_LIBCALL_SRC_FILES_CEA = \
# 	libcall_utils \
# 	windows_stubs \
# 	call_convention

##
# TRACE
##
$(eval $(call defcunit,$(DYNAMIC_DIR)/trace,trace_postprocessing trace_loader,,CEA))

$(eval $(call defcunit,$(DDSE_DIR)/syscall_stubs,syscall_stubs,,CEA))

$(eval $(call defcunit,$(DDSE_DIR)/instruction_stubs,instruction_stubs,,CEA))

$(eval $(call defcunit,$(DDSE_DIR)/tainting,taint_types tainting,,CEA))

$(eval $(call defcunit,$(DDSE_DIR),policy_utils policy_engine path_predicate,\
	policy_type,CEA))

$(eval $(call defcunit,$(DDSE_DIR)/path_exploration,\
	dseException \
	tracesToTree exploration_type\
	conf_exploration historyAsTree \
	symbolicInput \
	inputFromFiles \
	invertChild check_trace traceAsFile \
	eipRewrite guideAsRandom uaf_detection \
	Parsing_gueb \
	criteriaAsDefault criteriaEIPRewrite criteriaAsUAF \
	guideAsBFS guideAsDFS \
	guideAsUAF guideAsStrcmp \
	dse,\
	typeCriteriaDSE typeGuideDSE typeHistoryDSE typeTraceDSE \
	guideAsPriority guideAsShortestPath,IMAG))

$(eval, $(call defcunit,$(DDSE_DIR)/examples,\
	call_ret generic_analyse opaque_predicate,,CEA))

##
# Examples
##


$(eval $(call defcunit,$(DYNAMIC_DIR)/examples,\
	flareon sploit1 \
	stat_analysis switch branch_coverage \
	summary_analysis,,CEA))

$(eval $(call defcunit,$(DYNAMIC_DIR),server_options drun,,CEA))

$(eval $(call defcunit,server,dba_io server_callback server,,CEA))

$(eval $(call defcunit,examples/mcount,\
	mcount_options mcount_main,\
	,CEA))


# Some targets to debug stuff
.phony: inspect mydebug mlifiles mlfiles dist
mlifiles:
	echo "MLI FILES"
	echo $(BINSEC_MLI_FILES)

mlfiles:
	echo "ML FILES"
	echo $(BINSEC_ML_FILES)

dirs:
	echo "Directories"
	echo $(DIRS)

dist:
	echo "Distribution"
	echo $(BINSEC_DISTRIB_FILES)


generated:
	echo "Generated files"
	echo $(GENERATED_FILES)

inspect: mlifiles mlfiles dirs dist generated


# Other misc root files

MAIN_ML_FILES = test.ml main.ml

BINSEC_LICENSE_CEA += \
	$(ROOT_MLI_FILES) \
	$(ROOT_ML_FILES) $(MAIN_ML_FILES) test.mli

BINSEC_DISTRIB_FILES += $(ROOT_ML_FILES) $(MAIN_ML_FILES)

SHARE_DIR = share
BINSEC_DISTRIB_FILES += $(SHARE_DIR)

LIB_ML_FILES = $(BINSEC_ML_FILES)

LIB_MLI_FILES = $(BINSEC_MLI_FILES)

ML_FILES = \
	$(LIB_ML_FILES) \
	$(MAIN_ML_FILES)

MLI_FILES = \
	$(LIB_MLI_FILES) \
	test.mli


LIB_CMO_FILES = $(LIB_ML_FILES:%.ml=%.cmo)
LIB_CMX_FILES = $(LIB_ML_FILES:%.ml=%.cmx)
LIB_CMI_FILES = $(LIB_MLI_FILES:%.mli=%.cmi)

CMO_FILES = $(ML_FILES:%.ml=%.cmo)
CMX_FILES = $(ML_FILES:%.ml=%.cmx)
CMI_FILES = $(MLI_FILES:%.ml=%.cmi)

CAMLINCLUDES = $(DIRS:%=-I %)
CAMLFILES = $(MLI_FILES) $(ML_FILES)
