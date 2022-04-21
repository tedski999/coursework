
.PHONY: test
test: conv-harness
	./$< 128 128 3 64 64

.PHONY: prof
prof: conv-harness-p
	./$< 128 128 3 64 64
	gprof -b $< gmon.out

.PHONY: debug
debug: conv-harness-d
	gdb --ex "break student_conv" \
		--ex "tui enable" \
		--ex "run" \
		--args ./$< 8 8 3 1 1

.PHONY: clean
clean:
	$(RM) conv-harness

conv-harness: conv-harness.c
	gcc -O3 -msse4 -fopenmp -o $@ $^

conv-harness-p: conv-harness.c
	gcc -Wall -pg -msse4 -fopenmp -o $@ $^

conv-harness-d: conv-harness.c
	gcc -Wall -g -Og -msse4 -fopenmp -o $@ $^
