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

AC_INIT()

has_or_die ()
{
    if test "$1" = no; then
        AC_MSG_ERROR($2)
    fi
}

has_lib_or_die ()
{
    if test "$1" = ""; then
        AC_MSG_ERROR($2)
    fi
}

AC_CHECK_PROG(MAKE, make, make, no)

AC_CHECK_PROG(OCAMLFIND, ocamlfind, ocamlfind, no)
has_or_die $OCAMLFIND "Could not find ocamlfind"

# ocamlc bytecode compiler
AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,no)
if test "$OCAMLC" = no; then
    AC_MSG_ERROR("Could not find ocamlc.")
else
    AC_CHECK_PROG(OCAMLCDOTOPT,ocamlc.opt,ocamlc.opt, no)
    if test "$OCAMLCDOTOPT" != no; then
        OCAMLC=$OCAMLCDOTOPT
    fi
    OCAMLBEST=byt
fi

AC_MSG_CHECKING([version of OCaml])
OCAMLCVERSION=`$OCAMLC -version`
LEAST_VERSION="4.02.1"
case $OCAMLCVERSION in
     4.02*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.03*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.04*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     *) AC_MSG_ERROR("Please use at least $LEAST_VERSION (found $OCAMLCVERSION).");;
esac


# ocamlopt native compiler
AC_CHECK_PROG(OCAMLOPT, ocamlopt, ocamlopt, no)

if test "$OCAMLOPT" = no; then
    AC_MSG_WARN("Bytecode compilation only.")
else
    AC_CHECK_PROG(OCAMLOPTDOTOPT,ocamlopt.opt,ocamlopt.opt,no)
    if test "$OCAMLOPTDOTOPT" != no; then
        OCAMLOPT=$OCAMLOPTDOTOPT
    fi
    OCAMLOPTVERSION=`$OCAMLOPT -version`
    OCAMLBEST=opt
    if test "$OCAMLOPTVERSION" != "$OCAMLCVERSION"; then
       AC_MSG_ERROR("OCaml bytecode and optimized compilers have version number conflicts ($OCAMLCVERSION) vs ($OCAMLOPTVERSION) . Should be the same")
    fi
fi

# ocamlbuild
AC_CHECK_PROG(OCAMLBUILD,ocamlbuild,ocamlbuild,no)
has_or_die $OCAMLBUILD "Could not find ocamlbuild"

OCB_OPTIONS="-use-ocamlfind -no-hygiene -lflags ../loader_ml.o -libs nums,str -I ."

AC_SUBST(OCAMLBUILD)
AC_SUBST(OCB_OPTIONS)


# ocamldep
AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep, no)
has_or_die $OCAMLDEP "Could not find ocamldep"
OCAMLDEPVERSION=`ocamldep -version | sed -e 's/ocamldep, version \(.*\)/\1/'`
if test "$OCAMLDEPVERSION" != "$OCAMLCVERSION"; then
       AC_MSG_ERROR("ocamldep version $OCAMLCVERSION conflicts with ocamlc version $OCAMLCVERSION. Should be the same.")
fi

# ocamllex
AC_CHECK_PROG(OCAMLLEX,ocamllex,ocamllex,no)
has_or_die $OCAMLLEX "Could not find ocamllex"

# menhir
AC_CHECK_PROG(MENHIR,menhir,menhir,no)
has_or_die $MENHIR  "Could not find menhir"

# Checking other libraries
# Each check updates the LIB_FOUND variable to "yes" or "no"
# and MYLIBPATH to the actual path found if needed
LIB_FOUND="no"
MYLIBPATH=""
check_lib ()
{
  LIB_FOUND="no"
  AC_MSG_CHECKING(for library $1)
  MYLIBPATH=$($OCAMLFIND query $1 2>/dev/null | tr -d '\r\n')
  has_lib_or_die $MYLIBPATH "Could not find $1"
  AC_MSG_RESULT($MYLIBPATH)
  if test "$MYLIBPATH" = ""; then
     LIB_FOUND="no"
  else
     LIB_FOUND="yes"
  fi
  return 0
}


check_lib "ocamlgraph"
OCAMLGRAPH=$MYLIBPATH
if test "$LIB_FOUND" = "no"; then
  AC_MSG_ERROR("ocamlgraph not found")
fi

check_lib "piqilib"
PIQILIB=$MYLIBPATH
if test "$LIB_FOUND" = "no"; then
  AC_MSG_ERROR("piqilib not found")
fi

check_lib "zarith"
ZARITH=$MYLIBPATH
if test "$LIB_FOUND" = "no"; then
  AC_MSG_ERROR("zarith not found")
fi

check_lib "ZMQ"
ZMQ=$MYLIBPATH
if test "$LIB_FOUND" = "no"; then
  AC_MSG_ERROR("zmq not found")
fi

check_lib "llvm"
LLVM=$MYLIBPATH
if test "$LIB_FOUND" = "no"; then
  AC_MSG_ERROR("llvm not found")
fi



AC_CHECK_PROG(PIQI, piqi, piqi, no)
has_or_die $PIQI "Could not find piqi"

AC_CHECK_PROG(PIQI2CAML, piqic-ocaml , piqic-ocaml, no)
has_or_die $PIQI2CAML "Could not find piqic-ocaml"

AC_CHECK_PROG(PROTOC, protoc, protoc, no)
has_or_die $PROTOC "Could not find protoc"



# Optional libraries #
## ocamldoc
AC_CHECK_PROG(OCAMLDOC, ocamldoc, ocamldoc, no)
if test "$OCAMLDOC" = no; then
    AC_MSG_WARN("Documentation generation disabled.")
else
    OCAMLDOCVERSION=`ocamldoc -version`
fi


AC_CHECK_PROG(CMAKE, cmake, cmake, no)
if test "$CMAKE" = no; then
   AC_MSG_WARN("You will not be able to compile PINSEC")
fi

## headache
AC_CHECK_PROG(HEADACHE, headache, headache, no)
if test "$HEADACHE" = no; then
    AC_MSG_WARN("License generation disabled.")
fi

AC_CHECK_PROG(DOT, dot, dot, no)
if test "$DOT" = no; then
    AC_MSG_WARN("dot not found: disabling documentation generation.")
fi

## SMT Provers ##
AC_CHECK_PROG(Z3, z3, z3, no)
if test "$Z3" = no; then
    AC_MSG_WARN("z3 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(CVC4, cvc4, cvc4, no)
if test "$CVC4" = no; then
    AC_MSG_WARN("cvc4 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(YICES, yices-smt2, yices-smt2, no)
if test "$YICES" = no; then
    AC_MSG_WARN("yices-smt2 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(BOOLECTOR, boolector, boolector, no)
if test "$BOOLECTOR" = no; then
    AC_MSG_WARN("boolector not found: some parts of binsec might not work.")
fi


# Substitutions #

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(MENHIR)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLFIND)
AC_SUBST(OCAMLBEST)
AC_SUBST(HEADACHE)
AC_SUBST(PIQI)
AC_SUBST(PIQI2CAML)
AC_SUBST(PROTOC)

AC_CONFIG_FILES([Config.mk], [chmod a-w Config.mk])
AC_OUTPUT()

depend_file="src/.depend"
AS_ECHO(".depend: touching ${depend_file}")
touch ${depend_file}

AS_ECHO("")
AS_ECHO("")
AS_ECHO("Configuration summary")
AS_ECHO("---------------------")
AS_ECHO("ocamlc: $OCAMLC $OCAMLCVERSION")
AS_ECHO("ocamlopt: $OCAMLOPT $OCAMLOPTVERSION")
AS_ECHO("ocamldep: $OCAMLDEP $OCAMLDEPVERSION")
AS_ECHO("ocamllex: $OCAMLLEX")
AS_ECHO("menhir: $MENHIR")
AS_ECHO("ocamldoc: $OCAMLDOC $OCAMLDOCVERSION")
AS_ECHO("target: $OCAMLBEST")
AS_ECHO("headache: $HEADACHE")
