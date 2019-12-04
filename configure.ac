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

AC_INIT()

has_or_die ()
{
    if test x"$1" = xno; then
        AC_MSG_ERROR($2)
    fi
}

has_lib_or_die ()
{
    if test x"$1" = "x"; then
        AC_MSG_ERROR($2)
    fi
}

AC_CHECK_PROG(MAKE, make, make, no)
has_or_die $MAKE "Could not find GNU make"

AC_MSG_CHECKING([GNU make version])
GNU_MAKE_LEAST_VERSION=3.82
GNU_MAKE_VERSION=`$MAKE --version | head -n 1 | cut -d' ' -f 3`
case $GNU_MAKE_VERSION in
     3.82*) AS_ECHO("Good! ($GNU_MAKE_VERSION)");;
     4.**) AS_ECHO("Good! ($GNU_MAKE_VERSION)");;
#     4.2.1.*) AS_ECHO("Good! ($GNU_MAKE_VERSION)");;
     *) AC_MSG_ERROR("Please use a GNU Make version >= $GNU_MAKE_LEAST_VERSION (found $GNU_MAKE_VERSION).");;
esac

AC_CHECK_PROG(OCAMLFIND, ocamlfind, ocamlfind, no)
has_or_die $OCAMLFIND "Could not find ocamlfind"

# ocamlc bytecode compiler
AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,no)
if test x"$OCAMLC" = xno; then
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
LEAST_VERSION="4.04.2"
MOST_VERSION="4.07.1"
case $OCAMLCVERSION in
     4.04*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.05*) AS_ECHO("Good! ($OCAMLCVERSION)");;
     4.06.0) AS_ECHO("Good! ($OCAMLCVERSION)");;     # tested & included 2019-07-25
     4.06.1) AS_ECHO("Good! ($OCAMLCVERSION)");;     # tested & included 2019-07-25
     4.07.0) AS_ECHO("Good! ($OCAMLCVERSION)");;     # tested & included 2019-07-25
     4.07.1) AS_ECHO("Good! ($OCAMLCVERSION)");;     # tested & included 2019-07-25
     *) AC_MSG_ERROR("Please use an OCaml version >= $LEAST_VERSION and <= $MOST_VERSION (found $OCAMLCVERSION).");;
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
AC_CHECK_PROG(OCAMLBUILD, ocamlbuild, ocamlbuild, no)

if test "$OCAMLBUILD" == "no"; then
   AC_MSG_WARN("Could not find ocamlbuild -- you will only be able to build BINSEC through make")
fi

OCB_OPTIONS="-use-ocamlfind -I ."

# ocamldep
AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep, no)
has_or_die $OCAMLDEP "Could not find ocamldep"
OCAMLDEPVERSION=`ocamldep -version | sed -e 's/ocamldep, version \(.*\)/\1/'`
if test x"$OCAMLDEPVERSION" != x"$OCAMLCVERSION"; then
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
if test x"$LIB_FOUND" = x"no"; then
  AC_MSG_ERROR("ocamlgraph not found")
fi

check_lib "piqilib"
PIQILIB=$MYLIBPATH
if test x"$LIB_FOUND" = x"no"; then
  AC_MSG_ERROR("piqilib not found")
fi

check_lib "zarith"
ZARITH=$MYLIBPATH
if test x"$LIB_FOUND" = x"no"; then
  AC_MSG_ERROR("zarith not found")
fi

check_lib "zmq"
ZMQ=$MYLIBPATH
if test x"$LIB_FOUND" = x"no"; then
  AC_MSG_ERROR("zmq not found")
fi

check_lib "llvm"
LLVM=$MYLIBPATH
if test x"$LIB_FOUND" = x"no"; then
  AC_MSG_ERROR("llvm not found")
fi



AC_CHECK_PROG(PIQI, piqi, piqi, no)
has_or_die $PIQI "Could not find piqi"

AC_CHECK_PROG(PIQI2CAML, piqic-ocaml , piqic-ocaml, no)
has_or_die $PIQI2CAML "Could not find piqic-ocaml"



# Optional libraries #
## ocamldoc
AC_CHECK_PROG(OCAMLDOC, ocamldoc, ocamldoc, no)
if test x"$OCAMLDOC" = xno; then
    AC_MSG_WARN("Documentation generation disabled.")
else
    OCAMLDOCVERSION=`ocamldoc -version`
fi


## headache
AC_CHECK_PROG(HEADACHE, headache, headache, no)
if test x"$HEADACHE" = xno; then
    AC_MSG_WARN("License generation disabled.")
fi

AC_CHECK_PROG(DOT, dot, dot, no)
if test x"$DOT" = xno; then
    AC_MSG_WARN("dot not found: disabling documentation generation.")
fi

## SMT Provers ##
AC_CHECK_PROG(Z3, z3, z3, no)
if test x"$Z3" = xno; then
    AC_MSG_WARN("z3 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(CVC4, cvc4, cvc4, no)
if test x"$CVC4" = xno; then
    AC_MSG_WARN("cvc4 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(YICES, yices-smt2, yices-smt2, no)
if test x"$YICES" = xno; then
    AC_MSG_WARN("yices-smt2 not found: some parts of binsec might not work.")
fi

AC_CHECK_PROG(BOOLECTOR, boolector, boolector, no)
if test x"$BOOLECTOR" = xno; then
    AC_MSG_WARN("boolector not found: some parts of binsec might not work.")
fi

AC_CONFIG_COMMANDS([depend], [touch ${depend_file}], [depend_file="src/.depend"])

PIQI_DIR="piqi"
AC_CONFIG_COMMANDS([piqi_directory], [mkdir -p $dir],[dir="src/$PIQI_DIR"])


AC_CONFIG_COMMANDS([version],[cp VERSION $src_dir],[src_dir="src"])

# See https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Particular-Programs.html
AC_PROG_AWK
AC_PROG_MKDIR_P
AC_PROG_INSTALL

# Substitutions #
AC_SUBST(OCAMLBUILD)
AC_SUBST(OCB_OPTIONS)
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
AC_SUBST(PIQI_DIR)
AC_SUBST(AWK)
AC_SUBST(MKDIR_P)
AC_SUBST(INSTALL)

AC_CONFIG_FILES([Config.mk], [chmod a-w Config.mk])
AC_OUTPUT()


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
