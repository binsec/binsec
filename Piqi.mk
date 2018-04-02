##########################################################################
#  This file is part of Binsec.                                          #
#                                                                        #
#  Copyright (C) 2016-2017                                               #
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

.SUFFIXES: .piqi .proto .pb.cc

%_piqi.ml %_ext.ml: %.piqi
	$(PP) 'PIQIML $@'
	$(PIQI2CAML) $(PIQI2CAML_FLAGS) $<

$(PIQI_DIR)/%.piqi: $(PROTO_DIR)/%.proto
	$(PP) 'PIQI $@'
	$(PIQI) $(PIQI_FLAGS) -o $@ $<

CPP_PROTOBUF_DIR = $(PINSEC_DIR)/$(PINSEC_TYPES_DIR)/protobuf

.PHONY: pre-protoc

pre-protoc:
	$(MKDIR) $(CPP_PROTOBUF_DIR)

%.pb.cc: src/proto/%.proto
	$(PP) 'PROTOC $@'
	$(PROTOC) $(PROTOC_FLAGS) $<

PROTOC_FILES = $(PROTO_SRC_FILES:%=$(CPP_PROTOBUF_DIR)/%.pb.cc)

$(PROTOC_FILES) : pre-protoc

PROTO_LSRC_FILES = $(PROTO_FILES:%=$(BINSEC_DIR)/%)

protoc: pre-protoc $(PROTO_LSRC_FILES)
	$(PROTOC) $(PROTOC_FLAGS) $(PROTO_LSRC_FILES)

clean::
	$(RRM) $(CPP_PROTOBUF_DIR)

.PHONY: prepiqi
prepiqi:
	$(MKDIR) $(PIQI_DIR)

$(PIQI_FILES): prepiqi
$(PIQI_ML_FILES) : $(PIQI_FILES)
piqi-ml : $(PIQI_ML_FILES)
