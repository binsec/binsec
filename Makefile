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

default: binsec

ifneq (, $(shell which opam 2> /dev/null))

define install_deps
	$(shell dune external-lib-deps --missing $(1) 2>&1 \
	      | grep -e "opam install")
endef

OCAML_COMPILER ?= $(shell opam switch list | grep -m 1 -oe "ocaml-system[^ ]*")

_opam:
	opam switch create . $(OCAML_COMPILER) --no-install
	opam install tuareg merlin ocp-indent user-setup -y
	opam user-setup install
	opam pin add . -n
	opam install binsec --deps-only --with-test --with-doc -y

switch: _opam

else

switch:
	$(error "Please install opam.")

endif

ifeq (, $(shell which dune 2> /dev/null))

define check_dune
	$(error "Please install dune or run 'make switch'.")
endef

else

define clean_build
	dune clean
endef

endif

.PHONY: default switch install uninstall binsec test doc clean

binsec:
	$(call check_dune)
	$(call install_deps,@install)
	dune build @install

install: binsec
	dune install $(INSTALL_FLAGS)

uninstall:
	$(call check_dune)
	dune uninstall $(INSTALL_FLAGS)

test: binsec
	$(call install_deps,@runtest)
	dune build @runtest

doc:
	$(call check_dune)
	$(call install_deps,@doc)
	dune build @doc
	@echo "Documentation available @ _build/default/_doc/_html/"

clean::
	$(call clean_build)
