project(pinsec)
cmake_minimum_required(VERSION 3.0)
if(CMAKE_VERSION VERSION_GREATER 3.1)
    cmake_policy(SET CMP0054 NEW)
endif()

#=============== GLOBALS VARS ================
# ---- Looking for the Pintool directory
find_path(PIN_ROOT_DIR
  NAMES source/include/pin/pin.H
  PATHS $ENV{PIN_ROOT_DIR}
  DOC "Pin's base directory"
)
if(NOT PIN_ROOT_DIR)
  message(FATAL_ERROR "Pin not found!\n Please set the environment variable PIN_ROOT_DIR to the base directory of the pin library.\n")
endif(NOT PIN_ROOT_DIR)
message(STATUS "PIN_ROOT_DIR: ${PIN_ROOT_DIR}")
if(WIN32)
  find_path(JANSSON_DIR PATHS $ENV{JANSSON_DIR} DOC "Jansson's base directory")
  if(NOT JANSSON_DIR)
    message(FATAL_ERROR "Jansson directory not found!\n Please set the environment variable JANSSON_DIR to the base directory of the jansson library.\n")
  endif(NOT JANSSON_DIR)
  message(STATUS "JANSSON_DIR: ${JANSSON_DIR}")
  find_path(PROTOBUF_DIR PATHS $ENV{PROTOBUF_DIR} DOC "Protobuf's base directory")
  if(NOT PROTOBUF_DIR)
    message(FATAL_ERROR "Protobuf not found!\n Please set the environment variable PROTOBUF_DIR to the base directory of the protobuf library.\n")
  endif(NOT PROTOBUF_DIR)
  message(STATUS "PROTOBUF_DIR: ${PIN_ROOT_DIR}")
endif(WIN32)
#----------------------

#---- Check compiler version
if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "5.0")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fabi-version=2")
    endif()
endif()
#---------------------------

#Set the target architecture
set(PIN_CPU_ARCH "ia32")  #ia32e (for 64bits)
set(PIN_CPU_ARCH_LONG "ia32") #intel64 (for 64 bits)
#===============================================


#=============== DEFINITIONS =================
#Set definitions for the Pintool
set(PIN_DEFINITIONS "")
list(APPEND PIN_DEFINITIONS BIGARRAY_MULTIPLIER=1 USING_XED)

if("${PIN_CPU_ARCH}" STREQUAL "ia32e")
  list(APPEND PIN_DEFINITIONS TARGET_IA32E HOST_IA32E)
elseif("${PIN_CPU_ARCH}" STREQUAL "ia32")
  list(APPEND PIN_DEFINITIONS TARGET_IA32 HOST_IA32)
endif("${PIN_CPU_ARCH}" STREQUAL "ia32e")

if(UNIX)
  list(APPEND PIN_DEFINITIONS TARGET_LINUX)
elseif(WIN32)
  list(APPEND PIN_DEFINITIONS TARGET_WINDOWS _WIN32 _SECURE_SCL=0 _CRT_SECURE_NO_DEPRECATE _CPPUNWIND)
endif()
#=============================================


#============== INCLUDES ===============
find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

set(PIN_INCLUDE_DIRS
  ${PIN_ROOT_DIR}/extras/xed-${PIN_CPU_ARCH_LONG}/include
  ${PIN_ROOT_DIR}/source/include/pin
  ${PIN_ROOT_DIR}/source/include/pin/gen
  ${PIN_ROOT_DIR}/extras/components/include
  ${LOCAL_INCLUDE_DIR}
)
include_directories(${PIN_INCLUDE_DIRS})

if(UNIX)
  #ZMQ specific
  include_directories("${CMAKE_SOURCE_DIR}/lib/zmq/include")
elseif(WIN32)
  set(CMAKE_CONFIGURATION_TYPES Release)
  include_directories("${JANSSON_DIR}/build/include")
  include_directories("${PROTOBUF_DIR}/src")
else()
endif()
#=========================================


#Generate protobuf files
if(NOT EXISTS "${CMAKE_SOURCE_DIR}/src/types/protobuf/trace.pb.h") #Assume that if one does not exists none of them are
  message(WARNING "Protobuf files not generated! Please use make cpp in proto/ directory.")
  #execute_process(COMMAND protoc --proto_path=../../../proto --cpp_out=../src/types/protobuf ../../../proto/common.proto ../../../proto/dba.proto ../../../proto/config.proto ../../../proto/instruction.proto ../../../proto/syscall.proto ../../../proto/libcall.proto ../../../proto/message.proto ../../../proto/trace.proto)
endif()


# ========== SET FLAGS =========
if(UNIX)
  set(PIN_COMPILE_FLAGS "-Wall -Werror -MMD -Wno-unknown-pragmas -std=c++11 -O3 -fomit-frame-pointer -fno-strict-aliasing")

  set(PIN_VERSION_SCRIPT ${PIN_ROOT_DIR}/source/include/pin/pintool.ver)
  set(PIN_LINKER_FLAGS "-Wl,--no-undefined -Wl,--hash-style=sysv -shared -Wl,-Bsymbolic -Wl,--version-script=${PIN_VERSION_SCRIPT}")

  #Add -m32 for compilation and linking if needed
  if("${PIN_CPU_ARCH}" STREQUAL "ia32")
    set(PIN_COMPILE_FLAGS "${PIN_COMPILE_FLAGS} -m32")
    set(PIN_LINKER_FLAGS "${PIN_LINKER_FLAGS} -m32")
  endif()
elseif(WIN32)
  string(REPLACE "/EHsc" "" CMAKE_CXX_FLAGS_INIT ${CMAKE_CXX_FLAGS_INIT})
  set(PIN_COMPILE_FLAGS "/MT /nologo /EHs- /Eha- /wd4530 /GR- /GS- /Gy /Zi /Wall")
  set(PIN_LINKER_FLAGS "/DLL /MACHINE:X86 /ENTRY:Ptrace_DllMainCRTStartup@12 /SAFESEH:NO /INCREMENTAL:NO /MANIFEST:NO /OPT:NOREF /NOLOGO /EXPORT:main /NODEFAULTLIB")
  if("${PIN_CPU_ARCH}" STREQUAL "ia32")
    #Ok
  else()
    message(FATAL_ERROR "Pintool not meant to be compiled for other arch than x86 for now..\n")
  endif()
endif()
#=================================

#========== Set Source files ==========
#Put the whole together
file(GLOB SOURCE_FILES
    "./src/*"
    "./src/types/*"
    "./src/types/protobuf/*"
    "./src/inout/*"
    "./src/server/*"
    "./src/lib/json2pb/*"
)
#======================================

#============= LIBS DIRS =============
set(PIN_LIBRARY_DIRS
  ${PIN_ROOT_DIR}/extras/xed-${PIN_CPU_ARCH_LONG}/lib
  ${PIN_ROOT_DIR}/${PIN_CPU_ARCH_LONG}/lib
  ${PIN_ROOT_DIR}/${PIN_CPU_ARCH_LONG}/lib-ext
)
link_directories(${PIN_LIBRARY_DIRS})

if(UNIX)
  link_directories("${CMAKE_SOURCE_DIR}/lib/zmq/lib")
elseif(WIN32)
  link_directories("${JANSSON_DIR}/build/lib/Release")
  link_directories("${PROTOBUF_DIR}/vsprojects/Release")
endif()
#==========================================

add_library(${PROJECT_NAME} SHARED ${SOURCE_FILES})

#=========== LIBS files ===========
if(UNIX)
  target_link_libraries(${PROJECT_NAME} pin xed pindwarf dl protobuf jansson zmq)
elseif(WIN32)
  target_link_libraries(${PROJECT_NAME} pin libcmt libcpmt libprotobuf pinvm libxed jansson kernel32 ntdll-32)
endif()
#==================================


set_target_properties(${PROJECT_NAME} PROPERTIES
  COMPILE_DEFINITIONS "${PIN_DEFINITIONS}"
  COMPILE_FLAGS "${PIN_COMPILE_FLAGS}"
  LINK_FLAGS "${PIN_LINKER_FLAGS}"
)
