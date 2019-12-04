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

# You need to include Config.mk whenever using this file
# It defines some variables used here, e.g, PIQI_DIR

GENERATED_FILES =

BINSEC_LICENSE_CEA =
BINSEC_LICENSE_CEA_IMAG =
BINSEC_LICENSE_IMAG =
BINSEC_LICENSE_EXTERNAL_CHLIPALA =


BUILD_FILES = \
	_tags Makefile Lib.mk Targets.mk
BINSEC_DISTRIB_FILES += $(BUILD_FILES)
BINSEC_LICENSE_CEA += $(BUILD_FILES)

BINSEC_ML_FILES =
BINSEC_MLI_FILES =
DIRS =

#
# $1: list of ML FILES to add
# $2: list of MLI FILES
define append_src_files =
BINSEC_ML_FILES  += $(1)
BINSEC_MLI_FILES += $(2)
endef


# Definition of a compilation unit, with some parser/lexer inside
# The main thing is that these may depend on local modules, so we need to take that into account
# This notion covers both directories or
# plugin-like functionalities with independent CLI
# $1 : name of the directory (full path from root)
# $2 : source files basenames (which may need the parser)
# $3 : interface files basenames (i.e. .mli only - no ml)
# $4 : parser ml dependency (should come before the parser in link)
# $5 : parser files (i.e., .mly files)
# $6 : lexer files  (i.e., .mll files)
# $7 : license
define defcunit_parser =
$(1)_dir = $(1)
$(1)_src_files := $(2)
$(1)_int_files := $(3)
# parsers may have local dependencies, which need come before in link command
$(1)_parser_dep_files := $(4)  
$(1)_parser_files := $(5)     
$(1)_lexer_files  := $(6)
$(1)_generated_ml_files  := \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_parser_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_lexer_files))) 
# ocamllex does not generate mli files, so no need to add these files below
$(1)_generated_mli_files := \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_parser_files))) 
$(1)_ml_files := \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_parser_dep_files))) \
	$$($(1)_generated_ml_files) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_src_files)))
$(1)_mli_files := \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_int_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_parser_dep_files))) \
	$$($(1)_generated_mli_files) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_src_files)))
# we need to keep track of generated files for dependency computation through ocamldep
GENERATED_FILES += $$($(1)_generated_mli_files) $$($(1)_generated_ml_files) 
$(eval $(call append_src_files,$$($(1)_ml_files),$$($(1)_mli_files)))
# The bundle ia all the files we need to distribute when release time comes
$(1)_bundle = \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_parser_dep_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_parser_dep_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .ml,$$($(1)_src_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_src_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mli,$$($(1)_int_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mly,$$($(1)_parser_files))) \
	$$(addprefix $$($(1)_dir)/, $$(addsuffix .mll,$$($(1)_lexer_files))) 

BINSEC_DISTRIB_FILES += $$($(1)_bundle)
BINSEC_LICENSE_$(7)  += $$($(1)_bundle)
DIRS += $$($(1)_dir)
endef


# Definition of a compilation unit
# This notion covers both directories or
# plugin-like functionalities with independent CLI
# $1 : name of the directory (full path from root)
# $2 : source files basenames
# $3 : interface files basenames (i.e. mli only - no ml)
# $4 : license
define defcunit =
$(eval $(call defcunit_parser,$(1),$(2),$(3),,,,$(4)))
endef
