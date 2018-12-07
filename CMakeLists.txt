cmake_minimum_required(VERSION 3.4)
project(grc-compiler)

set(CMAKE_CXX_FLAGS "-O3")

find_package(LLVM REQUIRED CONFIG)
find_package(BISON)
find_package(FLEX)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

BISON_TARGET(MyParser src/Parser.y ${CMAKE_CURRENT_BINARY_DIR}/Parser.cpp
             COMPILE_FLAGS)
FLEX_TARGET(MyScanner src/Scanner.l  ${CMAKE_CURRENT_BINARY_DIR}/Scanner.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyScanner MyParser)

include_directories(includes ${CMAKE_CURRENT_BINARY_DIR})

file(GLOB FONTES "src/*.cpp")

add_executable(
    grcc ${FONTES}
    ${BISON_MyParser_OUTPUTS}
    ${FLEX_MyScanner_OUTPUTS})

install(TARGETS grcc RUNTIME DESTINATION bin)

#llvm_map_components_to_libnames(llvm_libs bitreader core support)
llvm_map_components_to_libnames(llvm_libs support core irreader)

target_link_libraries(grcc PUBLIC "LLVM-7" ${llvm_libs} )
