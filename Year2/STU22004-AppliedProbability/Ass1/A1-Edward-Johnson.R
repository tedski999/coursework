
SAMPLE.COUNT <- 10000  # Number of samples per iteration
ITERATION.COUNT <- 200 # Number of simulations to find an average

cat("\nCalculating mean probability that two random integers are coprime using", ITERATION.COUNT, "simulations with", SAMPLE.COUNT, "samples each...\n")
readline(prompt="Hit [enter] to continue...")

# Iterate ITERATION.COUNT times so we can find an average
results <- vector("integer", ITERATION.COUNT)
for (i in 1:ITERATION.COUNT) {
	cat("Performing iteration ", i, "/", ITERATION.COUNT, "... ", sep="")

	# Count number of coprimes by randomly selecting pairs
	count <- 0
	for (j in 1:SAMPLE.COUNT) {

		# Sample 2 random integers
		pair <- ceiling(runif(2, min=0, max=.Machine$integer.max))

		# Find GCD using the Euclidean algorithm
		# Using this, pair[2] will become the GCD
		while (pair[1]) {
			temp <- pair[1]
			pair[1] <- pair[2] %% pair[1]
			pair[2] <- temp
		}

		# The pair are coprime if their GCD is 1
		if (pair[2] == 1)
			count <- count + 1
	}

	# Add the count to the vector of results
	results[i] = count
	cat("Number of coprimes found in sample:", results[i], "\n")
}

# Calculate the average and probability
results.mean = mean(results)
cat("Mean number of coprimes found:", results.mean, "\n")
cat("Mean probability of two integers being coprimes:", results.mean / SAMPLE.COUNT, "\n")

