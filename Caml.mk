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

.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES: .o .c
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly

.cmo.o:
	$(PP) "COBJ $@"
	$(CAMLBYT) -custom -output-obj -o $@ $<

.c.o:
	$(CAMLBYT) -ccopt "-fPIC -o $@" -c $<

.ml.cmo:
	$(PP_BYT) $@
	$(CAMLBYT)  $(CAMLINCLUDES) $(CAMLFLAGS) $(CAMLWARNINGS) -c $<

.mli.cmi:
	$(PP_BYT) $@
	$(CAMLBYT)  $(CAMLINCLUDES) -c $<

.ml.cmx:
	$(PP_OPT) $@
	$(CAMLBIN) $(CAMLINCLUDES) $(CAMLFLAGS) $(CAMLWARNINGS) -c $<

.mly.ml:
	$(PP_YACC) $@
	$(CAMLYAC) $(CAMLYACOPTS) $<

.mly.mli:
	$(PP_YACC) $@
	$(CAMLYAC)  $(CAMLYACOPTS) $<

.mll.ml:
	$(PP_LEX) $@
	$(CAMLLEX) $(CAMLLEXOPTS) $<

# Generic clean up
cleandir::
	$(RRM) *.cm[ioxa] *.cmxa *.o *.a *.annot *.obj *.lib *~ .*~ a.out .\#*


configure:: cleandir

# Rebuilding dependencies
depend:: beforedepend
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

include .depend
