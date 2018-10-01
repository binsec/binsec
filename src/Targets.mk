##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2016-2018                                               #
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

# You need to includ Config.mk whenever using this file
# It defines some variables used here, e.g, PIQI_DIR

OPTION_FILES =
GENERATED_FILES =

BINSEC_LICENSE_CEA =
BINSEC_LICENSE_CEA_IMAG =
BINSEC_LICENSE_IMAG =
BINSEC_LICENSE_EXTERNAL_CHLIPALA =


BUILD_FILES = \
	_tags Makefile Targets.mk
BINSEC_DISTRIB_FILES += $(BUILD_FILES)
BINSEC_LICENSE_CEA += $(BUILD_FILES)


DBA_DIR = dba
DBA_SRC_FILES = dba dba_printer dba_types dba_to_formula errors dba_utils dba_visitor
DBA_INT_FILES =
DBA_ML_FILES = $(DBA_SRC_FILES:%=$(DBA_DIR)/%.ml)
DBA_MLI_FILES = \
	$(DBA_SRC_FILES:%=$(DBA_DIR)/%.mli) \
	$(DBA_INT_FILES:%=$(DBA_DIR)/%.mli)
DBA_FILES = $(DBA_ML_FILES) $(DBA_MLI_FILES)
BINSEC_DISTRIB_FILES += $(DBA_FILES)
BINSEC_LICENSE_CEA += $(DBA_FILES)


UTILS_DIR = utils
UTILS_SRC_FILES = \
	colors \
	binary_loc \
	directive \
	cfg \
	dhunk \
	instruction \
	network_io \
	instParsing
UTILS_ML_FILES = $(UTILS_SRC_FILES:%=$(UTILS_DIR)/%.ml)
UTILS_MLI_FILES = \
	$(UTILS_INT_FILES:%=$(UTILS_DIR)/%.mli) \
	$(UTILS_ML_FILES:%.ml=%.mli)


BASE_DIR = base
BASE_SRC_FILES = \
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
	utils
BASE_INT_FILES = sigs
BASE_ML_FILES = $(BASE_SRC_FILES:%=$(BASE_DIR)/%.ml)
BASE_MLI_FILES = \
	$(BASE_INT_FILES:%=$(BASE_DIR)/%.mli) \
	$(BASE_ML_FILES:%.ml=%.mli)

# Group all utilities into one set
ALL_UTILS_ML_FILES = \
	$(BASE_ML_FILES) \
	$(UTILS_ML_FILES)
ALL_UTILS_MLI_FILES = \
	$(BASE_MLI_FILES) \
	$(UTILS_MLI_FILES)

BINSEC_DISTRIB_FILES += \
	$(ALL_UTILS_ML_FILES) $(ALL_UTILS_MLI_FILES)

BINSEC_LICENSE_CEA += \
	$(ALL_UTILS_ML_FILES) $(ALL_UTILS_MLI_FILES)


##n
# Loader
##
LOADER_DIR = loader
LOADER_SRC_FILES = \
	loader_types loader_buf \
	loader_elf loader_dump loader_pe \
	loader loader_utils
LOADER_INT_FILES = loader_sigs
LOADER_ML_FILES = $(LOADER_SRC_FILES:%=$(LOADER_DIR)/%.ml)
LOADER_MLI_FILES = \
	$(LOADER_INT_FILES:%=$(LOADER_DIR)/%.mli) \
	$(LOADER_ML_FILES:%.ml=%.mli)
LOADER_CAML_FILES = $(LOADER_ML_FILES) $(LOADER_MLI_FILES)

BINSEC_DISTRIB_FILES += $(LOADER_CAML_FILES)
BINSEC_LICENSE_CEA +=  $(LOADER_CAML_FILES)

##
# Disassembly & subdirs
##
DISASM_DIR = disasm

# Disasm_options depends on X86Types ... this should be done better
OPTION_FILES += $(DISASM_DIR)/disasm_options

X86_DIR = $(DISASM_DIR)/x86
X86_INT_FILES = x86Types
X86_EXT_SRC_FILES = lreader x86Util x86pp
X86_OWN_SRC_FILES = predba x86Instruction x86decoder x86toDba
X86_SRC_FILES = \
	$(X86_EXT_SRC_FILES) \
	$(X86_OWN_SRC_FILES)
X86_ML_FILES = $(X86_SRC_FILES:%=$(X86_DIR)/%.ml)
X86_MLI_FILES = \
	$(X86_INT_FILES:%=$(X86_DIR)/%.mli) \
	$(X86_SRC_FILES:%=$(X86_DIR)/%.mli)
X86_FILES = $(X86_MLI_FILES) $(X86_ML_FILES)

BINSEC_DISTRIB_FILES += $(X86_FILES)

ARM_DIR = $(DISASM_DIR)/arm
ARM_SRC_FILES = armToDba
ARM_MLI_FILES = $(ARM_SRC_FILES:%=$(ARM_DIR)/%.mli)
ARM_ML_FILES = $(ARM_SRC_FILES:%=$(ARM_DIR)/%.ml)
ARM_FILES = $(ARM_MLI_FILES) $(ARM_ML_FILES)

BINSEC_DISTRIB_FILES += $(ARM_FILES)

BINSEC_LICENSE_CEA +=  \
	$(X86_DIR)/lreader.ml $(X86_DIR)/lreader.mli \
	$(X86_OWN_SRC_FILES:%=$(X86_DIR)/%.ml) \
	$(X86_OWN_SRC_FILES:%=$(X86_DIR)/%.mli) \
	$(ARM_FILES)

BINSEC_EXTERNAL = \
	$(X86_EXT_SRC_FILES:%=$(X86_DIR)/%.ml) \
	$(X86_EXT_SRC_FILES:%=$(X86_DIR)/%.mli)

SIMP_DIR = $(DISASM_DIR)/simplify

SIMP_OPTIONS = $(SIMP_DIR)/simplification_options

OPTION_FILES += $(SIMP_OPTIONS)

SIMP_SRC_FILES = \
	simplification_dba_utils \
	simplification_dba_block \
	simplification_dba_prog \
	simplification_dba

SIMP_ML_FILES = $(SIMP_SRC_FILES:%=$(SIMP_DIR)/%.ml)
SIMP_MLI_FILES = $(SIMP_INT_FILES) $(SIMP_SRC_FILES:%=$(SIMP_DIR)/%.mli)
SIMP_FILES = $(SIMP_MLI_FILES) $(SIMP_ML_FILES)

BINSEC_LICENSE_CEA +=  $(SIMP_FILES)
BINSEC_DISTRIB_FILES +=  $(SIMP_FILES)

DISASM_OWN_SRC_FILES = \
	decode_utils \
	disasm_core \
	disasm_cfg \
	disasm \

DISASM_SRC_FILES = \
	$(DISASM_EXT_SRC_FILES) \
	$(DISASM_OWN_SRC_FILES)
DISASM_BASE_ML_FILES = # $(DISASM_DIR)/disasm_types.ml
DISASM_BASE_MLI_FILES = # $(DISASM_DIR)/disasm_types.mli
DISASM_ML_FILES = $(DISASM_SRC_FILES:%=$(DISASM_DIR)/%.ml)
DISASM_MLI_FILES = \
	$(DISASM_ML_FILES:%.ml=%.mli) \
	$(DISASM_INT_FILES:%=$(DISASM_DIR)/%.mli)

DISASM_FILES = \
	$(DISASM_MLI_FILES) $(DISASM_ML_FILES) \
	$(DISASM_BASE_MLI_FILES) $(DISASM_BASE_ML_FILES)

BINSEC_DISTRIB_FILES += $(DISASM_FILES)

BINSEC_LICENSE_CEA += $(DISASM_FILES)


##
# LLVM
##
LLVM_DIR = llvm
LLVM_SRC_FILES = \
	transfer_functions \
	generic_decoder_sig \
	generic_decoder llvm_decoder
LLVM_INT_FILES = $(LLVM_SRC_FILES)
LLVM_ML_FILES = $(LLVM_SRC_FILES:%=$(LLVM_DIR)/%.ml)
LLVM_MLI_FILES = $(LLVM_INT_FILES:%=$(LLVM_DIR)/%.mli)
LLVM_FILES = \
	$(LLVM_ML_FILES) \
	$(LLVM_MLI_FILES)

BINSEC_LICENSE_CEA += $(LLVM_FILES)
BINSEC_DISTRIB_FILES += $(LLVM_FILES)

##
# Formula
##

FORMULA_DIR = formula
OPTION_FILES += $(FORMULA_DIR)/formula_options
FORMULA_SRC_FILES = \
	formula formula_utils \
	formula_to_smtlib \
	smtlib_to_formula \
	formula_pp \
	prover \
	formula_transformation solver \
	formula_main
FORMULA_INT_FILES = $(FORMULA_SRC_FILES)
FORMULA_ML_FILES = $(FORMULA_SRC_FILES:%=$(FORMULA_DIR)/%.ml)
FORMULA_MLI_FILES = \
	$(FORMULA_INT_FILES:%=$(FORMULA_DIR)/%.mli)
FORMULA_FILES = \
	$(FORMULA_MLI_FILES) \
	$(FORMULA_ML_FILES)

BINSEC_DISTRIB_FILES += $(FORMULA_FILES)
BINSEC_LICENSE_CEA += $(FORMULA_FILES)


##
# SMT
##

SMT_DIR = smtlib
SMT_PRE_SRC_FILES = locations
SMT_PRE_ML_FILES = $(SMT_PRE_SRC_FILES:%=$(SMT_DIR)/%.ml)
SMT_SRC_FILES = smtlib_pp smt_model smtlib_utils
SMT_INT_FILES = $(SMT_SRC_FILES) locations smtlib
SMT_PARSER_FILES = smtlib_parser
SMT_LEXER_FILES = smtlib_lexer
SMT_GENERATED_ML_FILES = \
	$(SMT_PARSER_FILES:%=$(SMT_DIR)/%.ml) \
	$(SMT_LEXER_FILES:%=$(SMT_DIR)/%.ml)

SMT_GENERATED_MLI_FILES = \
	$(SMT_PARSER_FILES:%=$(SMT_DIR)/%.mli)

SMT_ML_FILES = \
	$(SMT_PRE_ML_FILES) \
	$(SMT_GENERATED_ML_FILES) \
	$(SMT_SRC_FILES:%=$(SMT_DIR)/%.ml)

SMT_MLI_FILES = \
	$(SMT_GENERATED_MLI_FILES) \
	$(SMT_INT_FILES:%=$(SMT_DIR)/%.mli)

GENERATED_FILES += \
	$(SMT_GENERATED_MLI_FILES) \
	$(SMT_GENERATED_ML_FILES)

SMT_FILES = \
	$(SMT_PRE_ML_FILES) \
	$(SMT_SRC_FILES:%=$(SMT_DIR)/%.ml) \
	$(SMT_INT_FILES:%=$(SMT_DIR)/%.mli) \
	$(SMT_PARSER_FILES:%=$(SMT_DIR)/%.mly) \
	$(SMT_LEXER_FILES:%=$(SMT_DIR)/%.mll)

BINSEC_DISTRIB_FILES += $(SMT_FILES)
BINSEC_LICENSE_CEA += $(SMT_FILES)

##
# Kernel
##

KERNEL_DIR = kernel
# KERNEL_OPTIONS = kernel_options
KERNEL_SRC_FILES = kernel_options kernel_functions kernel_core
KERNEL_ML_FILES = $(KERNEL_SRC_FILES:%=$(KERNEL_DIR)/%.ml)
KERNEL_MLI_FILES = \
	$(KERNEL_ML_FILES:%.ml=%.mli) \
	$(KERNEL_INT_FILES:%=$(KERNEL_DIR)/%.mli)
KERNEL_FILES = $(KERNEL_ML_FILES) $(KERNEL_MLI_FILES)

BINSEC_DISTRIB_FILES += $(KERNEL_FILES)
BINSEC_LICENSE_CEA += $(KERNEL_FILES)

# OPTION_FILES += $(KERNEL_OPTIONS:%=$(KERNEL_DIR)/%)


##
# Ast
##
AST_DIR = ast
AST_SRC_FILES = instr_cfg cfgraph ast_builder
AST_ML_FILES = $(AST_SRC_FILES:%=$(AST_DIR)/%.ml)
AST_MLI_FILES = \
	$(AST_ML_FILES:%.ml=%.mli) \
	$(AST_INT_FILES:%=$(AST_DIR)/%.mli)
AST_FILES = $(AST_ML_FILES) $(AST_MLI_FILES)

BINSEC_DISTRIB_FILES += $(AST_FILES)
BINSEC_LICENSE_CEA += $(AST_FILES)


##
# Static common
##
STATIC_DIR = static
OPTION_FILES += $(STATIC_DIR)/static_options

STATIC_SRC_FILES = pmap static_types static_utils
STATIC_INT_FILES = $(STATIC_SRC_FILES)
STATIC_ML_FILES = $(STATIC_SRC_FILES:%=$(STATIC_DIR)/%.ml)
STATIC_MLI_FILES = $(STATIC_INT_FILES:%=$(STATIC_DIR)/%.mli)
STATIC_FILES = \
	$(STATIC_ML_FILES) \
	$(STATIC_MLI_FILES)

BINSEC_DISTRIB_FILES += $(STATIC_FILES)
BINSEC_LICENSE_CEA += $(STATIC_FILES)

# Static types
STATIC_TYPES_DIR = $(STATIC_DIR)/types
STATIC_TYPES_SRC_FILES = \
	smt_bitvectors region_bitvector
STATIC_TYPES_INT_FILES = $(STATIC_TYPES_SRC_FILES)
STATIC_TYPES_ML_FILES  = $(STATIC_TYPES_SRC_FILES:%=$(STATIC_TYPES_DIR)/%.ml)
STATIC_TYPES_MLI_FILES = $(STATIC_TYPES_INT_FILES:%=$(STATIC_TYPES_DIR)/%.mli)
STATIC_TYPES_FILES = \
	$(STATIC_TYPES_ML_FILES) \
	$(STATIC_TYPES_MLI_FILES)

BINSEC_DISTRIB_FILES += $(STATIC_TYPES_FILES)
BINSEC_LICENSE_CEA += $(STATIC_TYPES_FILES)


##
# Simulation
#
SIMULATION_DIR = $(STATIC_DIR)/simulation
OPTION_FILES += $(SIMULATION_DIR)/simulate_options
SIMULATION_SRC_FILES = \
	simulate_utils \
	concrete_state concrete_eval \
	simulate
SIMULATION_ML_FILES = $(SIMULATION_SRC_FILES:%=$(SIMULATION_DIR)/%.ml)
SIMULATION_MLI_FILES = \
	$(SIMULATION_ML_FILES:%.ml=%.mli) \
	$(SIMULATION_INT_FILES:%=$(SIMULATION_DIR)/%.mli)

SIMULATION_FILES = \
	$(SIMULATION_ML_FILES) \
	$(SIMULATION_MLI_FILES)

BINSEC_DISTRIB_FILES += $(SIMULATION_FILES)
BINSEC_LICENSE_CEA += $(SIMULATION_FILES)

##
# Interpreter
#
INTERPRETER_DIR = $(STATIC_DIR)/interpreter
OPTION_FILES += $(INTERPRETER_DIR)/simulation
INTERPRETER_SRC_FILES = concrete
INTERPRETER_ML_FILES = $(INTERPRETER_SRC_FILES:%=$(INTERPRETER_DIR)/%.ml)
INTERPRETER_MLI_FILES = \
	$(INTERPRETER_ML_FILES:%.ml=%.mli) \
	$(INTERPRETER_INT_FILES:%=$(INTERPRETER_DIR)/%.mli)

INTERPRETER_FILES = \
	$(INTERPRETER_ML_FILES) \
	$(INTERPRETER_MLI_FILES)

BINSEC_DISTRIB_FILES += $(INTERPRETER_FILES)
BINSEC_LICENSE_CEA += $(INTERPRETER_FILES)


##
# Abstract interpretation
##
AI_DIR = $(STATIC_DIR)/ai
OPTION_FILES += $(AI_DIR)/ai_options
AI_BASE_SRC_FILES =  normalize_instructions
AI_BASE_ML_FILES = $(AI_BASE_SRC_FILES:%=$(AI_DIR)/%.ml)
AI_BASE_MLI_FILES = $(AI_BASE_SRC_FILES:%=$(AI_DIR)/%.mli)
AI_SRC_FILES = \
	display ai_utils \
	backward_analysis \
	normalize_predicate high_level_predicate \
	reduced_product nonrelational \
	ai_results \
	ai
AI_INT_FILES = ai_sigs $(AI_SRC_FILES)
AI_ML_FILES = $(AI_SRC_FILES:%=$(AI_DIR)/%.ml)
AI_MLI_FILES = $(AI_INT_FILES:%=$(AI_DIR)/%.mli)

AI_DOMAINS_DIR = $(AI_DIR)/domains
AI_DOMAINS_SRC_FILES = domain_common \
	domain_interval range kset union_find taint
AI_DOMAINS_INT_FILES = $(AI_DOMAINS_SRC_FILES)
AI_DOMAINS_ML_FILES = $(AI_DOMAINS_SRC_FILES:%=$(AI_DOMAINS_DIR)/%.ml)
AI_DOMAINS_MLI_FILES = $(AI_DOMAINS_INT_FILES:%=$(AI_DOMAINS_DIR)/%.mli)

ALL_AI_ML_FILES = \
	$(AI_BASE_ML_FILES) \
	$(AI_DOMAINS_ML_FILES) \
	$(AI_ML_FILES)
ALL_AI_MLI_FILES = \
	$(AI_MLI_FILES) $(AI_BASE_MLI_FILES) $(AI_DOMAINS_MLI_FILES)

AI_FILES = \
	$(ALL_AI_ML_FILES) \
	$(ALL_AI_MLI_FILES)

BINSEC_DISTRIB_FILES += $(AI_FILES)
BINSEC_LICENSE_CEA += $(AI_FILES)


DYNAMIC_DIR = dynamic

##
# TRACE
##
DTRACE_DIR = $(DYNAMIC_DIR)/trace
OPTION_FILES += $(DTRACE_DIR)/trace_config \
	$(DTRACE_DIR)/trace_options
DTRACE_BASE_SRC_FILES = trace_type
DTRACE_BASE_ML_FILES = $(DTRACE_BASE_SRC_FILES:%=$(DTRACE_DIR)/%.ml)
DTRACE_BASE_MLI_FILES = $(DTRACE_BASE_SRC_FILES:%=$(DTRACE_DIR)/%.mli)
DTRACE_SRC_FILES = trace_postprocessing trace_loader # trace_visitor
DTRACE_INT_FILES = $(DTRACE_SRC_FILES)
DTRACE_ML_FILES = $(DTRACE_SRC_FILES:%=$(DTRACE_DIR)/%.ml)
DTRACE_MLI_FILES = $(DTRACE_INT_FILES:%=$(DTRACE_DIR)/%.mli)

##
# Examples
##

EXAMPLE_DIR = $(DYNAMIC_DIR)/examples

EXAMPLE_SRC_FILES = flareon sploit1 \
	stat_analysis switch branch_coverage \
	summary_analysis
EXAMPLE_INT_FILES = $(EXAMPLE_SRC_FILES)
EXAMPLE_ML_FILES = $(EXAMPLE_SRC_FILES:%=$(EXAMPLE_DIR)/%.ml)
EXAMPLE_MLI_FILES = $(EXAMPLE_INT_FILES:%=$(EXAMPLE_DIR)/%.mli)
EXAMPLE_FILES = $(EXAMPLE_ML_FILES) $(EXAMPLE_MLI_FILES)

# Not added to BINSEC_DISTRIB_FILES: they are added
# through DYNAMIC_ML_FILES and DYNAMIC_MLI_FILES
BINSEC_LICENSE_CEA += $(EXAMPLE_FILES)


DDSE_DIR = $(DYNAMIC_DIR)/dse
OPTION_FILES += $(DDSE_DIR)/dse_options
DDSE_BASE_SRC_FILES = \
	path_predicate_env \
	path_predicate_optim path_predicate_formula \
	path_predicate_utils
DDSE_BASE_INT_FILES = $(DDSE_BASE_SRC_FILES)
DDSE_BASE_ML_FILES = $(DDSE_BASE_SRC_FILES:%=$(DDSE_DIR)/%.ml)
DDSE_BASE_MLI_FILES = \
	$(DDSE_BASE_INT_FILES:%=$(DDSE_DIR)/%.mli)

DDSE_SRC_FILES = \
	policy_utils policy_engine path_predicate
DDSE_INT_FILES = policy_type $(DDSE_SRC_FILES)
DDSE_ML_FILES = $(DDSE_SRC_FILES:%=$(DDSE_DIR)/%.ml)
DDSE_MLI_FILES = \
	$(DDSE_INT_FILES:%=$(DDSE_DIR)/%.mli)


DTAINT_DIR = $(DDSE_DIR)/tainting
DTAINT_SRC_FILES = taint_types tainting
DTAINT_INT_FILES = $(DTAINT_SRC_FILES)
DTAINT_ML_FILES = $(DTAINT_SRC_FILES:%=$(DTAINT_DIR)/%.ml)
DTAINT_MLI_FILES = $(DTAINT_INT_FILES:%=$(DTAINT_DIR)/%.mli)

DDSE_SYSCALL_DIR  = $(DDSE_DIR)/syscall_stubs
DDSE_SYSCALL_SRC_FILES = syscall_stubs
DDSE_SYSCALL_INT_FILES = $(DDSE_SYSCALL_SRC_FILES)
DDSE_SYSCALL_ML_FILES = $(DDSE_SYSCALL_SRC_FILES:%=$(DDSE_SYSCALL_DIR)/%.ml)
DDSE_SYSCALL_MLI_FILES = $(DDSE_SYSCALL_INT_FILES:%=$(DDSE_SYSCALL_DIR)/%.mli)

DDSE_ISTUBS_DIR  = $(DDSE_DIR)/instruction_stubs
DDSE_ISTUBS_SRC_FILES = instruction_stubs
DDSE_ISTUBS_INT_FILES = $(DDSE_ISTUBS_SRC_FILES)
DDSE_ISTUBS_ML_FILES = $(DDSE_ISTUBS_SRC_FILES:%=$(DDSE_ISTUBS_DIR)/%.ml)
DDSE_ISTUBS_MLI_FILES = $(DDSE_ISTUBS_INT_FILES:%=$(DDSE_ISTUBS_DIR)/%.mli)

DDSE_LIBCALL_DIR = $(DDSE_DIR)/libcall_stubs
DDSE_LIBCALL_SRC_FILES_CEA_IMAG = \
	libc_stubs libcall_stubs
DDSE_LIBCALL_SRC_FILES_CEA = \
	libcall_utils \
	windows_stubs \
	call_convention
DDSE_LIBCALL_SRC_FILES = \
	$(DDSE_LIBCALL_SRC_FILES_CEA) \
	$(DDSE_LIBCALL_SRC_FILES_CEA_IMAG)
DDSE_LIBCALL_INT_FILES = libcall_types
DDSE_LIBCALL_ML_FILES = $(DDSE_LIBCALL_SRC_FILES:%=$(DDSE_LIBCALL_DIR)/%.ml)
DDSE_LIBCALL_MLI_FILES = \
	$(DDSE_LIBCALL_INT_FILES:%=$(DDSE_LIBCALL_DIR)/%.mli) \
	$(DDSE_LIBCALL_ML_FILES:%.ml=%.mli)

DDSE_EXAMPLES_DIR = $(DDSE_DIR)/examples
DDSE_EXAMPLES_SRC_FILES = \
	call_ret generic_analyse opaque_predicate

DDSE_EXAMPLES_INT_FILES = $(DDSE_EXAMPLES_SRC_FILES)
DDSE_EXAMPLES_ML_FILES = \
	$(DDSE_EXAMPLES_SRC_FILES:%=$(DDSE_EXAMPLES_DIR)/%.ml)

DDSE_EXAMPLES_MLI_FILES = \
	$(DDSE_EXAMPLES_INT_FILES:%=$(DDSE_EXAMPLES_DIR)/%.mli)



DPATH_DIR = $(DDSE_DIR)/path_exploration
DPATH_BASE_SRC_FILES = fdInput
DPATH_BASE_ML_FILES = $(DPATH_BASE_SRC_FILES:%=$(DPATH_DIR)/%.ml)
DPATH_BASE_MLI_FILES = $(DPATH_BASE_SRC_FILES:%=$(DPATH_DIR)/%.mli)
DPATH_SRC_FILES = \
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
	dse
DPATH_INT_FILES = \
	typeCriteriaDSE typeGuideDSE typeHistoryDSE typeTraceDSE \
	guideAsPriority guideAsShortestPath
DPATH_ML_FILES = $(DPATH_SRC_FILES:%=$(DPATH_DIR)/%.ml)
DPATH_MLI_FILES = \
	$(DPATH_INT_FILES:%=$(DPATH_DIR)/%.mli) \
	$(DPATH_ML_FILES:%.ml=%.mli)
DPATH_FILES = \
	$(DPATH_ML_FILES)  $(DPATH_MLI_FILES) \
	$(DPATH_BASE_ML_FILES)  $(DPATH_BASE_MLI_FILES)

DYNAMIC_FILES = drun

DYNAMIC_ML_FILES = \
	$(DTRACE_BASE_ML_FILES) \
	$(DPATH_BASE_ML_FILES) \
	$(DDSE_BASE_ML_FILES) \
	$(DDSE_LIBCALL_ML_FILES) \
	$(DTRACE_ML_FILES) \
	$(DDSE_SYSCALL_ML_FILES) \
	$(DDSE_ISTUBS_ML_FILES) \
	$(DTAINT_ML_FILES) \
	$(DDSE_ML_FILES) \
	$(DPATH_ML_FILES) \
	$(DDSE_EXAMPLES_ML_FILES) \
	$(EXAMPLE_ML_FILES) \
	$(DYNAMIC_FILES:%=$(DYNAMIC_DIR)/%.ml)

DYNAMIC_MLI_FILES = \
	$(DDSE_BASE_MLI_FILES) \
	$(DTRACE_BASE_MLI_FILES) \
	$(DTAINT_MLI_FILES) \
	$(DTRACE_MLI_FILES) \
	$(DDSE_LIBCALL_MLI_FILES) \
	$(DDSE_SYSCALL_MLI_FILES) \
	$(DDSE_ISTUBS_MLI_FILES) \
	$(DDSE_MLI_FILES) \
	$(DPATH_BASE_MLI_FILES) \
	$(DPATH_MLI_FILES) \
	$(DDSE_EXAMPLES_MLI_FILES) \
	$(EXAMPLE_MLI_FILES) \
	$(DYNAMIC_FILES:%=$(DYNAMIC_DIR)/%.mli)

DYNAMIC_DIRS = \
	$(DDSE_EXAMPLES_DIR) \
	$(DDSE_LIBCALL_DIR) \
	$(DDSE_SYSCALL_DIR) \
	$(DDSE_ISTUBS_DIR) \
	$(DTRACE_DIR) \
	$(DDSE_DIR) \
	$(DTAINT_DIR) \
	$(DPATH_DIR) \
	$(EXAMPLE_DIR) \
	$(DYNAMIC_DIR)

BINSEC_DISTRIB_FILES += $(DYNAMIC_ML_FILES) $(DYNAMIC_MLI_FILES)

# Licenses
BINSEC_LICENSE_CEA += \
	$(DTRACE_BASE_ML_FILES) \
	$(DTRACE_BASE_MLI_FILES) \
	$(DTRACE_ML_FILES) \
	$(DTRACE_MLI_FILES) \
	$(DDSE_LIBCALL_SRC_FILES_CEA:%=$(DDSE_LIBCALL_DIR)/%.ml) \
	$(DDSE_LIBCALL_SRC_FILES_CEA:%=$(DDSE_LIBCALL_DIR)/%.mli) \
	$(DDSE_SYSCALL_ML_FILES) \
	$(DDSE_SYSCALL_MLI_FILES) \
	$(DDSE_ISTUBS_ML_FILES) \
	$(DDSE_ISTUBS_MLI_FILES) \
	$(DTAINT_MLI_FILES) \
	$(DTAINT_ML_FILES) \
	$(DDSE_ML_FILES) \
	$(DDSE_MLI_FILES) \
	$(DDSE_EXAMPLES_ML_FILES) \
	$(DDSE_EXAMPLES_MLI_FILES)

BINSEC_LICENSE_CEA_IMAG += \
	$(DDSE_LIBCALL_SRC_FILES_CEA_IMAG:%=$(DDSE_LIBCALL_DIR)/%.ml) \
	$(DDSE_LIBCALL_SRC_FILES_CEA_IMAG:%=$(DDSE_LIBCALL_DIR)/%.mli)

BINSEC_LICENSE_IMAG += $(DPATH_FILES)

##
# SSE
##

SSE_DIR = sse
OPTION_FILES +=  $(SSE_DIR)/sse_options
SSE_SRC_FILES = \
	sse_symbolic sse_graph \
	sse_prune \
	sse_types sse_utils \
	sse_smt \
	sse
SSE_INT_FILES = $(SSE_SRC_FILES)
SSE_ML_FILES  = $(SSE_SRC_FILES:%=$(SSE_DIR)/%.ml)
SSE_MLI_FILES = $(SSE_INT_FILES:%=$(SSE_DIR)/%.mli)

SSE_FILES = \
	$(SSE_ML_FILES) \
	$(SSE_MLI_FILES)

BINSEC_DISTRIB_FILES += $(SSE_FILES)
BINSEC_LICENSE_CEA += $(SSE_FILES)

###
# Backwards reasoner
###

BW_DIR = backwards
OPTION_FILES += $(BW_DIR)/bw_options
BW_SRC_FILES = \
	opaque \
	bw_main
BW_INT_FILES = $(BW_SRC_FILES)
BW_ML_FILES  = $(BW_SRC_FILES:%=$(BW_DIR)/%.ml)
BW_MLI_FILES = $(BW_INT_FILES:%=$(BW_DIR)/%.mli)

BW_FILES = \
	$(BW_ML_FILES) \
	$(BW_MLI_FILES)

BINSEC_DISTRIB_FILES += $(BW_FILES)
BINSEC_LICENSE_CEA += $(BW_FILES)

###
# Xtrasec
##

XTRASEC_DIR = xtrasec
OPTION_FILES +=  $(XTRASEC_DIR)/xtrasec_options
XTRASEC_SRC_FILES = \
	parsepin formula_decoder xtrasec
XTRASEC_INT_FILES =  $(XTRASEC_SRC_FILES)
XTRASEC_ML_FILES  = $(XTRASEC_SRC_FILES:%=$(XTRASEC_DIR)/%.ml)
XTRASEC_MLI_FILES = $(XTRASEC_INT_FILES:%=$(XTRASEC_DIR)/%.mli)

XTRASEC_FILES = \
	$(XTRASEC_ML_FILES) \
	$(XTRASEC_MLI_FILES)

BINSEC_DISTRIB_FILES += $(XTRASEC_FILES)
BINSEC_LICENSE_CEA += $(XTRASEC_FILES)

# Server
SERVER_DIR = server
OPTION_FILES += $(SERVER_DIR)/server_options
SERVER_SRC_FILES = dba_io server_callback server
SERVER_ML_FILES = $(SERVER_SRC_FILES:%=$(SERVER_DIR)/%.ml)
SERVER_MLI_FILES = $(SERVER_SRC_FILES:%=$(SERVER_DIR)/%.mli)
SERVER_FILES = $(SERVER_MLI_FILES) $(SERVER_ML_FILES)

BINSEC_DISTRIB_FILES += $(SERVER_FILES)
BINSEC_LICENSE_CEA += $(SERVER_FILES)


##
# Parsers
##
PARSER_DIR = parser
PARSER_PARSER_FILES = dbacsl_parser parser parser_infos policy_parser SMTParserWp
PARSER_LEXER_FILES = dbacsl_token lexer lexer_infos policy_token SMTLexerWp
PARSER_PRE_SRC_FILES = parse_helpers
PARSER_POST_SRC_FILES = parse_utils
PARSER_SRC_FILES = $(PARSER_PRE_SRC_FILES) $(PARSER_POST_SRC_FILES)
PARSER_GENERATED_ML_FILES = \
	$(PARSER_PARSER_FILES:%=$(PARSER_DIR)/%.ml) \
	$(PARSER_LEXER_FILES:%=$(PARSER_DIR)/%.ml)
PARSER_GENERATED_MLI_FILES = \
	$(PARSER_PARSER_FILES:%=$(PARSER_DIR)/%.mli)
PARSER_ML_FILES = \
	$(PARSER_PRE_SRC_FILES:%=$(PARSER_DIR)/%.ml) \
	$(PARSER_GENERATED_ML_FILES) \
	$(PARSER_POST_SRC_FILES:%=$(PARSER_DIR)/%.ml)
# Weirdly enough, ocamllex does not generate mli
PARSER_MLI_FILES = \
	$(PARSER_PRE_SRC_FILES:%=$(PARSER_DIR)/%.mli) \
	$(PARSER_PARSER_FILES:%=$(PARSER_DIR)/%.mli) \
	$(PARSER_POST_SRC_FILES:%=$(PARSER_DIR)/%.mli)
GENERATED_FILES += \
	$(PARSER_GENERATED_ML_FILES) \
	$(PARSER_GENERATED_MLI_FILES)

PARSER_FILES = \
	$(PARSER_PARSER_FILES:%=$(PARSER_DIR)/%.mly) \
	$(PARSER_LEXER_FILES:%=$(PARSER_DIR)/%.mll) \
	$(PARSER_SRC_FILES:%=$(PARSER_DIR)/%.ml) \
	$(PARSER_SRC_FILES:%=$(PARSER_DIR)/%.mli)

BINSEC_DISTRIB_FILES += $(PARSER_FILES)
BINSEC_LICENSE_CEA += $(PARSER_FILES)


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


BINPATCHER_DIR = binpatcher
OPTION_FILES += $(BINPATCHER_DIR)/binpatcher_options
BINPATCHER_SRC_FILES = binpatcher
BINPATCHER_INT_FILES = $(BINPATCHER_SRC_FILES)
BINPATCHER_ML_FILES = $(BINPATCHER_SRC_FILES:%=$(BINPATCHER_DIR)/%.ml)
BINPATCHER_MLI_FILES = $(BINPATCHER_INT_FILES:%=$(BINPATCHER_DIR)/%.mli)
BINPATCHER_FILES = \
	$(BINPATCHER_ML_FILES) \
	$(BINPATCHER_MLI_FILES)

BINSEC_DISTRIB_FILES += $(BINPATCHER_FILES)
BINSEC_LICENSE_CEA   += $(BINPATCHER_FILES)


# Other misc root files

ROOT_SRC_FILES = infos
ROOT_INT_FILES = infos
ROOT_ML_FILES = $(ROOT_SRC_FILES:%=%.ml)
ROOT_MLI_FILES = $(ROOT_INT_FILES:%=%.mli)
MAIN_ML_FILES = test.ml main.ml

BINSEC_LICENSE_CEA += \
	$(ROOT_MLI_FILES) \
	$(ROOT_ML_FILES) $(MAIN_ML_FILES) test.mli

BINSEC_DISTRIB_FILES += $(ROOT_ML_FILES) $(MAIN_ML_FILES)

OPTION_ML_FILES = $(OPTION_FILES:%=%.ml)
OPTION_MLI_FILES = $(OPTION_FILES:%=%.mli)

BINSEC_LICENSE_CEA += \
	$(OPTION_ML_FILES) \
	$(OPTION_MLI_FILES)

SHARE_DIR = share
BINSEC_DISTRIB_FILES += $(SHARE_DIR)

BINSEC_DISTRIB_FILES += $(OPTION_ML_FILES) $(OPTION_MLI_FILES)

LIB_ML_FILES = \
	$(BASE_ML_FILES) \
	$(LOADER_ML_FILES) \
	$(DBA_ML_FILES) \
	$(PIQI_ML_FILES) \
	config.ml \
	$(UTILS_ML_FILES) \
	$(KERNEL_ML_FILES) \
	$(ROOT_ML_FILES) \
	$(PARSER_ML_FILES) \
	$(OPTION_ML_FILES) \
	$(SMT_ML_FILES) \
	$(DISASM_BASE_ML_FILES) \
	$(FORMULA_ML_FILES) \
	$(STATIC_TYPES_ML_FILES) \
	$(AST_ML_FILES) \
	$(X86_ML_FILES) \
	$(ARM_ML_FILES) \
	$(SIMP_ML_FILES) \
	$(LLVM_ML_FILES) \
	$(DISASM_ML_FILES) \
	$(STATIC_ML_FILES) \
	$(SIMULATION_ML_FILES) \
	$(INTERPRETER_ML_FILES) \
	$(ALL_AI_ML_FILES) \
	$(DYNAMIC_ML_FILES) \
	$(SERVER_ML_FILES) \
	$(SSE_ML_FILES) \
	$(XTRASEC_ML_FILES) \
	$(BW_ML_FILES) \
	$(BINPATCHER_ML_FILES)

LIB_MLI_FILES = \
	$(KERNEL_MLI_FILES) \
	$(STATIC_MLI_FILES) \
	$(DBA_MLI_FILES) \
	$(LOADER_MLI_FILES) \
	$(FORMULA_MLI_FILES) \
	$(PARSER_MLI_FILES) \
	$(STATIC_TYPES_MLI_FILES) \
	$(SMT_MLI_FILES) \
	$(AST_MLI_FILES) \
	$(X86_MLI_FILES) \
	$(ARM_MLI_FILES) \
	$(SIMP_MLI_FILES) \
	$(BASE_MLI_FILES) \
	$(DISASM_BASE_MLI_FILES) \
	$(DISASM_MLI_FILES) \
	$(ALL_AI_MLI_FILES) \
	$(SIMULATION_MLI_FILES) \
	$(INTERPRETER_MLI_FILES) \
	$(DYNAMIC_MLI_FILES) \
	$(LLVM_MLI_FILES) \
	$(SERVER_MLI_FILES) \
	$(SSE_MLI_FILES) \
	$(XTRASEC_MLI_FILES) \
	$(BW_MLI_FILES) \
	$(BINPATCHER_MLI_FILES) \
	$(ROOT_MLI_FILES) \
	$(UTILS_MLI_FILES) \
	$(OPTION_MLI_FILES)

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

DIRS = \
	$(BASE_DIR) \
	$(UTILS_DIR) \
	$(DBA_DIR) \
	$(FORMULA_DIR) \
	$(AST_DIR) \
	$(X86_DIR) \
	$(SIMP_DIR) \
	$(DISASM_DIR) \
	$(STATIC_TYPES_DIR) \
	$(STATIC_DIR) \
	$(PARSER_DIR) \
	$(AI_DIR) $(AI_DOMAINS_DIR)\
	$(SIMULATION_DIR) \
	$(INTERPRETER_DIR) \
	$(LOADER_DIR) \
	$(DYNAMIC_DIRS) \
	$(SMT_DIR) $(SMT_PARSER_DIR) \
	$(EXAMPLE_DIR) \
	$(SERVER_DIR) \
	$(PIQI_DIR) \
	$(DSE_DIR) \
	$(ARM_DIR) \
	$(LLVM_DIR) \
	$(BINPATCHER_DIR) \
	$(SSE_DIR) \
	$(BW_DIR) \
	$(XTRASEC_DIR) \
	$(KERNEL_DIR)

CAMLINCLUDES = $(DIRS:%=-I %) -I .
CAMLFILES = $(MLI_FILES) $(ML_FILES)
