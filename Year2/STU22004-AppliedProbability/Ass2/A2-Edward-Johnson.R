
### QUESTION 1 ###

# This problem can be solved empirical as below,
# or we can figure it out mathematically:
# The chance of rolling a specific face is 1/n where n is the number of possible faces.
# Furthermore, the chance of rolling any m specific faces for n faces is m/n.
# However, the mean of this random variable can be expressed as its inverse of its chance: n/m.
# To begin with, there are 6 possible faces and 6 faces left to get.
# From before, we can see that is n=6 and m=6, giving us a mean chance of 6/6 (certain!) to roll a new face.
# Then we would have n=6 and m=5, for a mean chance of 6/5.
# Continuing this, we get est_count = 6/6 + 6/5 + 6/4 + 6/3 + 6/2 + 6/1 = 14.7
# Or more generally: n/n + n/n-1 + ... + n/2 + n/1

FACE.COUNT <- 6        # Number of faces on a die
SAMPLE.COUNT <- 10000  # Number of samples per iteration
ITERATION.COUNT <- 200 # Number of simulations to find an average

cat("\nCalculating mean number of rolls to get every face on a", FACE.COUNT, "sided die using", ITERATION.COUNT, "simulations with", SAMPLE.COUNT, "samples each...\n")
readline(prompt="Hit [enter] to continue...")

# Iterate ITERATION.COUNT time so we can find an average
results <- vector("double", ITERATION.COUNT)
for (i in 1:ITERATION.COUNT) {
	cat("Performing iteration ", i, "/", ITERATION.COUNT, "... ", sep="")

	# Count number of rolls taken to see every face SAMPLE.COUNT times
	count <- 0
	for (j in 1:SAMPLE.COUNT) {
		face.remaining = FACE.COUNT
		while (face.remaining) {
			count <- count + 1
			if (ceiling(runif(1, min=0, max=FACE.COUNT)) <= face.remaining)
				face.remaining <- face.remaining - 1
		}
	}

	# Add average number of rolls to results
	results[i] <- count / SAMPLE.COUNT
	cat("Average number of rolls to see every face:", results[i], "\n")
}

# Calculate the overall mean
results.mean <- mean(results)
cat("Overall mean number of rolls to see every face:", results.mean, "\n")


### QUESTION 2 ###

K.VALUES <- 6:10       # Values to be used for k
SAMPLE.COUNT <- 10000  # Number of samples per iteration
ITERATION.COUNT <- 200 # Number of simulations to find an average

cat("\nCalculating mean probability of guessing every card wrong from k randomly shuffled cards using", ITERATION.COUNT, "simulations with", SAMPLE.COUNT, "samples each...\n")
readline(prompt="Hit [enter] to continue...")

# Perform simulation for k=6,7,8,9,10
results <- vector()
for (k in K.VALUES) {
	cat("\nSimulating k = ", k, "...\n", sep="")
	cards <- 1:k

	# Iterate ITERATION.COUNT time so we can find an average for this value for k
	p <- vector("double", ITERATION.COUNT)
	for (i in 1:ITERATION.COUNT) {
		cat("Performing iteration ", i, "/", ITERATION.COUNT, "... ", sep="")

		# Count the number of times guessing randomly results in no correct guesses
		count <- 0
		for (j in 1:SAMPLE.COUNT) {
			guess <- sample(cards) # A guess is a random permutation of the cards
			if (!any(guess == cards)) # Don't count if any of the guesses match
				count <- count + 1
		}

		# Add average probability of completely incorrect guesses to results
		p[i] <- count / SAMPLE.COUNT
		cat("Average probability of being completely incorrect:", p[i], "\n")
	}

	# Calculate mean of the above simulations results
	p.mean <- mean(p)
	cat("Mean probability for k=", k, ": ", p.mean, "\n", sep="");
	results <- append(results, p.mean)
}

# Stats and Plot
cat("\nDrawing plot of results...\n");
plot(K.VALUES, results, type="b", main="Relation between the number of cards and the\n probability of incorrectly guessing every card", xlab="Number of cards", ylab="Est. Probability")

readline(prompt="\nHit [enter] to exit...")
graphics.off()

