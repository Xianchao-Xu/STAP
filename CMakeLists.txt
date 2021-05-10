cmake_minimum_required(VERSION 3.15 FATAL_ERROR)

project(STAP LANGUAGES CXX Fortran)

add_subdirectory(stap_f77)
add_subdirectory(stap_f90)
