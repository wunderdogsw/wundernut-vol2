cmake_minimum_required(VERSION 2.8)
set(CMAKE_CONFIGURATION_TYPES "Debug;Release")
project(muhkeuttaja)

enable_language(C)
enable_language(CXX)

include("defaults.cmake")

find_boost_program_options(TRUE)
find_boost_system(TRUE)
find_boost_thread(TRUE)

output_flags("DEBUG" on)

add_definitions(-DVERSION="r0")

include_directories("${PROJECT_SOURCE_DIR}/src")

add_executable(muhkeuttaja "src/main.cpp" "src/muhkeuttaja.cpp" "src/muhkeuttaja.hpp")
if(${MSVC})
  target_link_libraries(muhkeuttaja "WINMM")
else()
  target_link_libraries(muhkeuttaja ${BOOST_PROGRAM_OPTIONS_LIBRARY})
  target_link_libraries(muhkeuttaja ${BOOST_SYSTEM_LIBRARY})
  target_link_libraries(muhkeuttaja ${BOOST_THREAD_LIBRARY})
endif()
