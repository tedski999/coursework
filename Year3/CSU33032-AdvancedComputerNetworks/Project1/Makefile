# Web Proxy
# Copyright (C) 2022 Ted Johnson TCD 19335618 <edjohnso@tcd.ie>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

TARGET = webproxy

SRC_DIR    = src
OBJ_DIR    = obj
BIN_DIR    = bin
BUILD_TYPE = release

override CFLAGS += -Wall -pedantic
override LFLAGS +=

SRC_FILES := $(shell find $(SRC_DIR) -type f -name '*.c')
OBJ_FILES := $(SRC_FILES:$(SRC_DIR)/%.c=$(OBJ_DIR)/$(BUILD_TYPE)/%.o)
DEP_FILES := $(SRC_FILES:$(SRC_DIR)/%.c=$(OBJ_DIR)/$(BUILD_TYPE)/%.d)

# Shortcuts for building release builds
.PHONY: $(TARGET)
$(TARGET): $(BIN_DIR)/$(BUILD_TYPE)/$(TARGET)
	@echo -e "\n# Completed $(BUILD_TYPE) build."

# Build debug binaries
.PHONY: debug
debug:
	@echo -e "\n# Building debug binary..."
	make -B --no-print-directory BUILD_TYPE="debug" CFLAGS="-g"

# Run binary
.PHONY: run
run: $(TARGET)
	@echo -e "\n# Running $(BUILD_TYPE) build..."
	./$(BIN_DIR)/$(BUILD_TYPE)/$(TARGET)

# Remove the build directories
.PHONY: clean
clean:
	@echo -e "\n# Removing build files..."
	$(RM) -r $(OBJ_DIR) $(BIN_DIR)

# Build flowctl binary
$(BIN_DIR)/$(BUILD_TYPE)/$(TARGET): $(OBJ_FILES)
	@echo -e "\n# Linking $@..."
	@mkdir -p $(@D)
	$(CC) $^ $(LFLAGS) -o $@

-include $(DEP_FILES)

# Compile source files to object file
$(OBJ_DIR)/$(BUILD_TYPE)/%.o: $(SRC_DIR)/%.c $(SRC_DIR)/%.h Makefile
	@echo -e "\n# Compiling $< -> $@"
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@
