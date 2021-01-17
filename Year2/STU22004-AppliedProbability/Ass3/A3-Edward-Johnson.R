
### QUESTION 1 ###

DIST.AVG <- 24000      # Average lifetime in hours
K.VALUES <- 2:5        # Values to be used for k
ITERATION.COUNT <- 200 # Number of simulations to find an average
SAMPLE.COUNT <- 10000  # Number of samples per iteration

cat("\nCalculating mean percentage of one lamps lifetime over the total lifetime of k lamps using", ITERATION.COUNT, "simulations with", SAMPLE.COUNT, "samples each...\n")
readline(prompt="Hit [enter] to continue...")

# Perform simulation for k=2,3,4,5
results <- vector()
for (k in K.VALUES) {
	cat("\nSimulating k = ", k, "...\n", sep="")

	# Iterate ITERATION.COUNT times so we can find an average for this value for k
	p <- vector("double", ITERATION.COUNT)
	for (i in 1:ITERATION.COUNT) {
		cat("Performing iteration ", i, "/", ITERATION.COUNT, "... ", sep="")

		# Sample SAMPLE.COUNT times from an exponential distribution k
		# times to simulate k lamp lifetimes
		lifetimes <- vector("double", k)
		for (j in 1:k)
			lifetimes[j] <- sum(rexp(SAMPLE.COUNT, DIST.AVG)) / ITERATION.COUNT

		# Save 100 * (T1/T) to simulation results
		p[i] <- 100 * (lifetimes[1] / sum(lifetimes))
		cat(" Percentage: ", p[i], "\n")
	}

	# Calculate mean of the above simulations results
	p.mean <- mean(p)
	results <- append(results, p.mean)
	cat("Mean percentage for k=", k, ": ", p.mean, "\n", sep="");
}

# Plot results
cat("\nDrawing plot of results...\n");
plot(K.VALUES, results, type="b", main="Relation between the number of lamps and the\n percentage of the first lamp over the total lifetime of k lamps", xlab="Number of lamps", ylab="Percentage")


### QUESTION 2 ###

DIST.AVG <- 0
A <- 1
B <- 2
ITERATION.COUNT <- 200 # Number of simulations to find an average
SAMPLE.COUNT <- 10000  # Number of samples per iteration

cat("\nCalculating the standard deviation of a normal distribution with mean", DIST.AVG, "to maximize p(", A, "< X <", B, ") using", ITERATION.COUNT, "simulations with", SAMPLE.COUNT, "samples each...\n")
readline(prompt="Hit [enter] to continue...")

results <- vector()
for (i in 1:ITERATION.COUNT) {
	cat("Performing iteration ", i, "/", ITERATION.COUNT, "... ", sep="")

	# Perform SAMPLE.COUNT number of samples of normal distributions with randomly
	# picked standard deviations. Keep the standard deviation that gave the largest p
	p.max <- 0
	s.best <- 0
	for (j in 1:SAMPLE.COUNT) {
		s <- runif(1, A, B) # s.best is between A and B, sample randomly
		p <- pnorm(B, DIST.AVG, s) - pnorm(A, DIST.AVG, s)
		if (p > p.max) {
			p.max <- p
			s.best <- s
		}
	}

	# Add to the list of our results
	results <- append(results, s.best)
	cat("Best standard deviation found:", s.best, "\n");
}

# Calculate the mean of the best standard deviations found
cat("Average best standard deviation found:", mean(results))

readline(prompt="\nHit [enter] to exit...")
graphics.off()


