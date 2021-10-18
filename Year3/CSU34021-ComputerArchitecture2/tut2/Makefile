TARGET = t2
CC     = g++
AS     = nasm
AFLAGS = -f elf64 -w+all -g
CFLAGS = -m64 -Wall -pedantic -g
LFLAGS = -m64 -no-pie

.PHONY: all
all: obj bin bin/t2
	@echo "Build complete!"

.PHONY: info
info:
	@echo "C++ compiler: $(CC) -c $(CFLAGS)"
	@echo "Assembler:    $(AS) $(AFLAGS)"
	@echo "Linker:       $(CC) $(LFLAGS)"

.PHONY: run
run: all
	./bin/$(TARGET)

# Manage build directories
.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -rf obj bin
obj:
	@mkdir -p obj
bin:
	@mkdir -p bin

# Link Object files
bin/$(TARGET): obj/$(TARGET).o obj/$(TARGET)Test.o
	@echo "Linking $@..."
	@$(CC) $^ $(LFLAGS) -o $@

# Assemble Assembly
obj/%.o: src/%.asm Makefile
	@echo "Assembling $< -> $@"
	@$(AS) $< $(AFLAGS) -o $@

# Compile C++
obj/%.o: src/%.cpp Makefile
	@echo "Compiling $< -> $@"
	@$(CC) -c $< $(CFLAGS) -o $@
