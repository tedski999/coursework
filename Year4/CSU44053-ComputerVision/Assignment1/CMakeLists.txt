cmake_minimum_required(VERSION 3.1)
project(draughts)

set(CMAKE_GXX_FLAGS "-Wall -Wextra -Wconversion -pedantic -std=gnu11")

find_package(OpenCV REQUIRED)

add_executable(${PROJECT_NAME}
	src/main.cpp
	src/Common.cpp
	src/MyApplication.cpp
	src/Part1.cpp
	src/Part2.cpp
	src/Part3.cpp
	src/Part4.cpp
	src/Part5.cpp
)

target_include_directories(${PROJECT_NAME} PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(${PROJECT_NAME} ${OpenCV_LIBS})
