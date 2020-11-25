TARGET = listset
CC     = gcc
CFLAGS = -Wall -pedantic

SRC_FILES := $(sort $(shell find . -name '*.c'))
INC_FILES := $(sort $(shell find . -name '*.h'))

.PHONY: all
all: $(TARGET)
	@echo "Build complete."

.PHONY: clean
clean:
	rm -f $(TARGET) $(TARGET)_submission.tar

.PHONY: submission_archive
submission_archive: $(SRC_FILES) $(INC_FILES) Makefile
	tar -cvf $(TARGET)_submission.tar $^

$(TARGET): $(SRC_FILES)
	$(CC) $^ $(CFLAGS) -o $@

