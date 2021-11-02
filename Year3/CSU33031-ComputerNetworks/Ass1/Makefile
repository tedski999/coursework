RUN_ARGS = 14285

# Project structure
TARGET  = subpub
SRC_DIR = src
TST_DIR = tst
OBJ_DIR = obj
BIN_DIR = bin

# Build options
CC = gcc
override CFLAGS += -Wall -pedantic
override LFLAGS +=

# Modify options when building tests
SUBDIR = release
BIN_SUBDIR = $(BIN_DIR)/$(SUBDIR)
OBJ_SUBDIR = $(OBJ_DIR)/$(SUBDIR)
SRC_FILES  = $(shell find $(SRC_DIR)/ -type f -name '*.c')

# Default: Test and build
.PHONY: all
all: build
#all: test build

# Build release  binary
.PHONY: build
build: $(BIN_SUBDIR)/$(TARGET)
	@echo -e "\n# Build complete."

# Build debug binary
.PHONY: debug
debug:
	@echo -e "\n# Building debug version..."
	make --no-print-directory build SUBDIR="debug" CFLAGS="-g"

# Run tests
.PHONY: test
test:
	@echo -e "\n# Building and running tests..."
	make --no-print-directory run \
		SUBDIR="testing" \
		CFLAGS="-D main=_main" \
		LFLAGS="-lcheck" \
		SRC_FILES="$(SRC_FILES) $(shell find $(TST_DIR)/ -type f -name '*.c')"
	@echo -e "\n# Testing complete."

# Run target
.PHONY: run
run: $(BIN_SUBDIR)/$(TARGET)
	@echo -e "\n# Running ./$(BIN_SUBDIR)/$(TARGET) $(RUN_ARGS)"
	./$(BIN_SUBDIR)/$(TARGET) $(RUN_ARGS)

# Remove the build directories
.PHONY: clean
clean:
	@echo -e "\n# Removing build directories..."
	rm -rf $(BIN_DIR) $(OBJ_DIR)

# Link object files into binary
$(BIN_SUBDIR)/$(TARGET): $(SRC_FILES:%.c=$(OBJ_SUBDIR)/%.o)
	@echo -e "\n# Linking $@..."
	@mkdir -p $(@D)
	$(CC) $^ $(LFLAGS) -o $@

# Compile source files to object file
$(OBJ_SUBDIR)/%.o: %.c Makefile
	@echo -e "\n# Compiling $< -> $@"
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
