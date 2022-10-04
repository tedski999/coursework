SRC_FILES = \
	src/main.cpp \
	src/yar.cpp \
	src/model.cpp \
	src/object.cpp \
	src/shader.cpp

prog: ${SRC_FILES}
	g++ $^ -lGL -lGLEW -lglfw -lassimp -o $@
