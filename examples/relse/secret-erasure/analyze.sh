#!/usr/bin/bash

SCRUBBING_FUNCTIONS=${SCRUBBING_FUNCTIONS:="LOOP MEMSET BZERO EXPLICIT_BZERO MEMSET_S VOLATILE_FUNC \
    PTR_TO_VOLATILE_LOOP PTR_TO_VOLATILE_MEMSET VOLATILE_PTR_LOOP VOLATILE_PTR_MEMSET \
    VOL_PTR_TO_VOL_LOOP VOL_PTR_TO_VOL_MEMSET \
    MEMORY_BARRIER_SIMPLE MEMORY_BARRIER_MFENCE MEMORY_BARRIER_LOCK MEMORY_BARRIER_PTR \
    WEAK_SYMBOLS"}

CLANG_VERSIONS="clang-3.0_3.0 clang-3.9_3.9 clang_7.1.0 clang_9.0.1 clang_11.0.1"
GCC_VERSIONS="gcc_5.4.0 gcc_6.2.0 gcc_7.2.0 gcc_8.3.0 gcc_10.2.0"
ALL_COMPILERS=${ALL_COMPILERS:="${CLANG_VERSIONS} ${GCC_VERSIONS}"}

OPTIMIZATIONS=${OPTIMIZATIONS:="O0 O1 O2 O3"}
NO_DSE_OPTIONS_CLANG="witnessO3 no-dse"
NO_DSE_OPTIONS_GCC="witnessO3 no-dse no-tree-dse no-dse-all"

ROOT=$(git root)
if [ $? -ne 0 ]; then
    echo "Cannot find the root of the current workspace"
    exit 1
fi
SRC="${ROOT}/examples/relse/secret-erasure"
BIN="${ROOT}/examples/relse/secret-erasure/bin"
mkdir -p ${BIN}

REF=${REF:="15ed83089660519858865d5a37b45b3a0e5f2195"}
ORIGIN="https://github.com/binsec/rel_bench/raw/${REF}/properties_vs_compilers/secret-erasure/bin"

# [run binary_file] Run binsec on the specified [binary_file]
run() {
    NAME="${1}"

    if [ ! -f ${BIN}/${NAME} ]; then
        wget -q -O "${BIN}/${NAME}" "${ORIGIN}/${NAME}"
        if [ $? -ne 0 ]; then
            RESULT="UNKNOWN"
            return
        fi
        chmod +x "${BIN}/${NAME}"
    fi

    CORE=$(mktemp --tmpdir "${NAME}.XXX.snapshot")

    make_coredump.sh "${CORE}" "${BIN}/${NAME}" > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        RESULT="UNKNOWN"
        return
    fi

    RES=$(binsec -sse -checkct -sse-script "${SRC}/mfence.ini,${SRC}/check.ini,${SRC}/libsym.ini" -sse-missing-symbol quiet -sse-depth 100000 -sse-timeout 60 "${CORE}" 2>&1)

    rm ${CORE}

    if [[ ${RES} =~ "sse:fatal" ]] || [[ ${RES} =~ "sse:error" ]]; then
        RESULT="ERROR"
    elif [[ ${RES} =~ "checkct:error" ]]; then
        RESULT="INSECURE"
    elif [[ ${RES} =~ "Program status is : secure" ]]; then
        RESULT="SECURE"
    else
        RESULT="UNKNOWN"
    fi
}

# [pp_results scrubbing_functions compilers optimization]
pp_results () {
    for scrubbing_function in ${1}
    do
        for compiler in ${2}
        do
            for optimization in ${3}
            do
                NAME="secret-erasure_${scrubbing_function}_${optimization}_${compiler}"
                run "${NAME}"
                echo "$NAME: ${RESULT}"
            done
        done
    done
}

wrong_usage() {
    echo "Usage: analyze.sh [ all-opt | all-no-dse ]"
    exit 1
}

if [ "${1}" = "all-opt" ]; then
    pp_results "${SCRUBBING_FUNCTIONS}" "${ALL_COMPILERS}" "${OPTIMIZATIONS}"
elif [ "${1}" = "all-no-dse" ]; then
    pp_results "${SCRUBBING_FUNCTIONS}" "${CLANG_VERSIONS}" "${NO_DSE_OPTIONS_CLANG}"
    pp_results "${SCRUBBING_FUNCTIONS}" "${GCC_VERSIONS}" "${NO_DSE_OPTIONS_GCC}"
else
    wrong_usage
fi

exit 0
