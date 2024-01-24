cmake_minimum_required(VERSION 3.20)
project(type_list VERSION 1.0.0)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "Prevented in-source built.")
endif()

add_library(type_list INTERFACE)
target_include_directories(type_list INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_features(type_list INTERFACE cxx_std_20)
