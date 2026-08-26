##########################################################################
#  This file is part of BINSEC.                                          #
#                                                                        #
#  Copyright (C) 2016-2026                                               #
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

BINSEC_VERSION ?= $(shell opam info --just-file ./binsec.opam -f version)

OCAMLFORMAT_VERSION ?= $(shell cut -d "=" -f 2 .ocamlformat)

_opam:
	@if [ -n "$(OCAML_COMPILER)" ]; then \
	  compiler="$(OCAML_COMPILER)"; \
	else \
	  compiler=$$(opam list ocaml-base-compiler --installed --coinstallable-with=binsec.$(BINSEC_VERSION) --columns=version --short | sort -V | tail -n 1); \
	  if [ -z "$$compiler" ]; then \
	    compiler=$$(opam list ocaml-base-compiler --coinstallable-with=binsec.$(BINSEC_VERSION) --columns=version --short | sort -V | tail -n 1); \
	    if [ -z "$$compiler" ]; then \
	      echo "No OCaml compiler coinstallable with binsec.$(BINSEC_VERSION) found."; \
	      exit 1; \
	    fi; \
	    read -p "No installed OCaml compiler found for binsec.$(BINSEC_VERSION); use OCaml $$compiler? [y/N] " answer; \
	    case "$$answer" in \
	      [yY]*) ;; \
	      *) echo "Aborted. Set OCAML_COMPILER to choose a different version."; exit 1;; \
	    esac; \
	  fi; \
	fi; \
	opam switch create . $$compiler --no-install
	opam pin add . -n
	opam install binsec --deps-only --with-test --with-doc -y

_opam-dev:
	opam install merlin ocamlformat=$(OCAMLFORMAT_VERSION) user-setup -y
	opam user-setup install

switch: _opam

switch-dev: switch _opam-dev

else

switch:
	$(error "Please install opam.")

switch-dev: switch

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

.PHONY: default switch switch-dev install uninstall binsec test doc clean

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
