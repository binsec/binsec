##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2016-2024                                               #
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

default: binsec

ifneq (, $(shell command -v opam 2> /dev/null))

OCAML_COMPILER ?= $(shell opam switch list 2> /dev/null |\
		    grep -m 1 -oe "ocaml-system[^ ]*" )

_opam:
	opam switch create . $(OCAML_COMPILER) --no-install
	opam pin add . -n
	opam install binsec --deps-only --with-test --with-doc -y
	opam install merlin ocamlformat=0.26.1 user-setup -y
	opam user-setup install

switch: _opam

else

switch:
	$(error "Please install opam.")

endif

ifeq (, $(shell command -v dune 2> /dev/null))

define check_dune
	$(error "Please install dune or run 'make switch'.")
endef

else

define clean_build
	dune clean
endef

endif

ifneq (, $(shell command -v ocamlformat 2> /dev/null))

define apply_ocamlformat
	$(shell dune build @fmt --auto-promote)
endef

endif

.PHONY: default switch install uninstall binsec test doc clean

binsec:
	$(call check_dune)
	$(call apply_ocamlformat)
	dune build @install

install: binsec
	dune install $(INSTALL_FLAGS)

uninstall:
	$(call check_dune)
	dune uninstall $(INSTALL_FLAGS)

test: binsec
	dune build @runtest

doc:
	$(call check_dune)
	dune build @doc
	@echo "Documentation available @ _build/default/_doc/_html/"

clean::
	$(call clean_build)
