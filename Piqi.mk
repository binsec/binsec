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

.SUFFIXES: .piqi .proto 

%_piqi.ml %_ext.ml: %.piqi
	$(PP) 'PIQIML $@'
	$(PIQI2CAML) $(PIQI2CAML_FLAGS) $<

# $(PIQI_DIR) is created by configure script
$(PIQI_DIR)/%.piqi: $(PROTO_DIR)/%.proto
	$(PP) 'PIQI $@'
	$(PIQI) $(PIQI_FLAGS) -o $@ $<

$(PIQI_ML_FILES): $(PIQI_FILES)

piqi-ml : $(PIQI_ML_FILES)

