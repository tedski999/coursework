# Flow Forwarding
# Copyright (C) 2021 Ted Johnson TCD 19335618 <edjohnso@tcd.ie>
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

FLOWCTL = flowctl
FLOWFWD = flowfwd

CC         = gcc
SRC_DIR    = src
OBJ_DIR    = obj
BIN_DIR    = bin
BUILD_TYPE = release

override CFLAGS += -Wall -pedantic
override LFLAGS +=

FLOWCTL_SRC_FILES := $(shell find $(SRC_DIR)/$(FLOWCTL) -type f -name '*.c')
FLOWCTL_OBJ_FILES := $(FLOWCTL_SRC_FILES:$(SRC_DIR)/%.c=$(OBJ_DIR)/$(BUILD_TYPE)/%.o)
FLOWFWD_SRC_FILES := $(shell find $(SRC_DIR)/$(FLOWFWD) -type f -name '*.c')
FLOWFWD_OBJ_FILES := $(FLOWFWD_SRC_FILES:$(SRC_DIR)/%.c=$(OBJ_DIR)/$(BUILD_TYPE)/%.o)
COMMON_SRC_FILES  := $(shell find $(SRC_DIR)/common -type f -name '*.c')
COMMON_OBJ_FILES  := $(COMMON_SRC_FILES:$(SRC_DIR)/%.c=$(OBJ_DIR)/$(BUILD_TYPE)/%.o)

# Default: Build all binaries
.PHONY: all
all: $(FLOWCTL) $(FLOWFWD)

# Shortcuts for building release builds
.PHONY: $(FLOWCTL)
$(FLOWCTL): $(BIN_DIR)/$(BUILD_TYPE)/$(FLOWCTL)
.PHONY: $(FLOWFWD)
$(FLOWFWD): $(BIN_DIR)/$(BUILD_TYPE)/$(FLOWFWD)

# Build debug binaries
.PHONY: debug
debug:
	@echo -e "\n# Building debug binaries..."
	make --no-print-directory BUILD_TYPE="debug" CFLAGS="-g"

# Remove the build directories
.PHONY: clean
clean:
	@echo -e "\n# Removing build files..."
	rm -rf $(OBJ_DIR) $(BIN_DIR)

# Build flowctl binary
$(BIN_DIR)/$(BUILD_TYPE)/$(FLOWCTL): $(FLOWCTL_OBJ_FILES) $(COMMON_OBJ_FILES)
	@echo -e "\n# Linking $@..."
	@mkdir -p $(@D)
	$(CC) $^ $(LFLAGS) -o $@

# Build flowfwd binary
$(BIN_DIR)/$(BUILD_TYPE)/$(FLOWFWD): $(FLOWFWD_OBJ_FILES) $(COMMON_OBJ_FILES)
	@echo -e "\n# Linking $@"
	@mkdir -p $(@D)
	$(CC) $^ $(LFLAGS) -o $@

# Compile source files to object file
$(OBJ_DIR)/$(BUILD_TYPE)/%.o: $(SRC_DIR)/%.c Makefile
	@echo -e "\n# Compiling $< -> $@"
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
