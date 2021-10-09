CC     = g++
AS     = nasm
AFLAGS = -f elf32 -w+all
CFLAGS = -m32 -Wall -pedantic
LFLAGS = -m32

.PHONY: all
all: obj bin bin/t1
	@echo "Build complete!"

.PHONY: info
info:
	@echo "C++ compiler: $(CC) -c $(CFLAGS)"
	@echo "Assembler:    $(AS) $(AFLAGS)"
	@echo "Linker:       $(CC) $(LFLAGS)"

.PHONY: run
run: all
	./bin/t1

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
bin/t1: obj/t1.o obj/t1Test.o
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
