TARGET = yar
CFLAGS = -Wall -pedantic -O2
LFLAGS = -lGL -lGLEW -lglfw -lassimp
SRC_FILES = \
	src/main.cpp \
	src/yar.cpp \
	src/model.cpp \
	src/object.cpp \
	src/shader.cpp

bin/$(TARGET): $(SRC_FILES:src/%.cpp=obj/%.o)
	@mkdir -p bin
	g++ $(LFLAGS) $^ -o $@

obj/%.o: src/%.cpp Makefile
	@mkdir -p obj
	g++ $(CFLAGS) -c $< -o $@
