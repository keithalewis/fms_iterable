# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build

# Include any dependencies generated for this target.
include CMakeFiles/fms_iterable.t.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/fms_iterable.t.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/fms_iterable.t.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/fms_iterable.t.dir/flags.make

CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o: CMakeFiles/fms_iterable.t.dir/flags.make
CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o: ../fms_iterable.t.cpp
CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o: CMakeFiles/fms_iterable.t.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o -MF CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o.d -o CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o -c /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/fms_iterable.t.cpp

CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/fms_iterable.t.cpp > CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.i

CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/fms_iterable.t.cpp -o CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.s

# Object files for target fms_iterable.t
fms_iterable_t_OBJECTS = \
"CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o"

# External object files for target fms_iterable.t
fms_iterable_t_EXTERNAL_OBJECTS =

fms_iterable.t: CMakeFiles/fms_iterable.t.dir/fms_iterable.t.cpp.o
fms_iterable.t: CMakeFiles/fms_iterable.t.dir/build.make
fms_iterable.t: CMakeFiles/fms_iterable.t.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable fms_iterable.t"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/fms_iterable.t.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/fms_iterable.t.dir/build: fms_iterable.t
.PHONY : CMakeFiles/fms_iterable.t.dir/build

CMakeFiles/fms_iterable.t.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/fms_iterable.t.dir/cmake_clean.cmake
.PHONY : CMakeFiles/fms_iterable.t.dir/clean

CMakeFiles/fms_iterable.t.dir/depend:
	cd /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2 /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2 /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build /mnt/c/users/kal/source/repos/keithalewis/fms_iterable2/build/CMakeFiles/fms_iterable.t.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/fms_iterable.t.dir/depend

