cmake_minimum_required (VERSION 3.16)

project (fms_iterable VERSION 1.20
                      DESCRIPTION "Iterators with operator bool() to detect end()."
                      LANGUAGES CXX)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable (fms_iterable.t fms_iterable.t.cpp fms_iterable.h)
target_compile_definitions(fms_iterable.t PUBLIC _DEBUG)
target_compile_options(fms_iterable.t PRIVATE -g -Wall -Werror -pedantic -Wextra -fsanitize=address)
target_link_options(fms_iterable.t PRIVATE -fsanitize=address)

enable_testing ()
add_test (NAME fms_iterable.t COMMAND fms_iterable.t )
