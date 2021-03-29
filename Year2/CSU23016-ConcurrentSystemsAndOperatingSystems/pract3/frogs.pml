
// Exactly N frogs and N + 1 stones.
#define NUM_OF_FROGS (4)
#define NUM_OF_STONES (NUM_OF_FROGS + 1)

// The possible types of frogs on a stone..
mtype = { RIGHT, LEFT, EMPTY }

// Initialise the frogs to their starting positions.
int empty_position = 2                                       // The current position of the only empty stone. Used to determine if a frog can move.
int frog_positions[NUM_OF_FROGS] = { 1, 3, 4, 5 }            // The current positions of the frogs. Note that index 0 = FROG1, index 1 = FROG2, etc...
mtype frog_types[NUM_OF_FROGS] = { RIGHT, LEFT, LEFT, LEFT } // The types of each of the frogs. Note that index 0 = FROG1, index 1 = FROG2, etc...

// The types of frogs in each position required for the trail to be successful.
mtype required_order[NUM_OF_STONES] = { LEFT, LEFT, LEFT, EMPTY, RIGHT }

// Prints the positions of every frog.
inline print_positions() {
	printf("EMPTY %d, FROG1@%d, FROG2@%d, FROG3@%d, FROG4@%d\n", empty_position, frog_positions[0], frog_positions[1], frog_positions[2], frog_positions[3])
}

// Moves a frog a distance, updating the empty position accordingly.
// Clearly, this should all be performed atomically.
inline move_frog(index, distance) {
	int destination = frog_positions[index] + distance

	// Announce our frog leaping intentions.
	printf("MOVE FROG%d FROM %d TO %d\n", index + 1, frog_positions[index], destination)

	// Let's make sure we haven't messed up somehow.
	assert(destination == empty_position)

	// Swap the frog and the empty position.
	empty_position = frog_positions[index]
	frog_positions[index] = destination

	// Output new frog positions.
	print_positions()
}

proctype frog_controller(int index) {
	int direction_multiplier

	// Determine which direction this frog is facing.
	if
	:: (frog_types[index] == RIGHT) -> direction_multiplier = 1 ; printf("START FROG %d AT %d GOING RIGHT\n", index + 1, frog_positions[index])
	:: (frog_types[index] == LEFT) -> direction_multiplier = -1 ; printf("START FROG %d AT %d GOING LEFT\n", index + 1, frog_positions[index])
	fi

	// Keep attempting to move forward forever.
	// It's possible to label this end to signify this is a valid end-state,
	// however we're instead using the -E flag with spin.
	atomic {
		do
		// If the stone in front of the frog is empty, move to it.
		:: (frog_positions[index] + 1 * direction_multiplier == empty_position) -> move_frog(index, 1 * direction_multiplier)
		// If the next stone in front of the frog is empty, move to it.
		:: (frog_positions[index] + 2 * direction_multiplier == empty_position) -> move_frog(index, 2 * direction_multiplier)
		od
	}
}

init {
	print_positions()

	// Start all the frogs.
	int i
	//atomic {
	for (i in frog_positions) {
		run frog_controller(i)
	}
	//}

	// Wait for all the frogs to give up.
	(timeout)
	printf("DONE!\n")

	// Checks every frog is in a valid position for its type.
	bool success = true
	for (i in frog_positions) {
		int order_index = frog_positions[i] - 1
		mtype expected_frog_type = required_order[order_index]
		mtype actual_frog_type = frog_types[i]
		if
		:: (expected_frog_type != actual_frog_type) -> success = false
		:: else -> skip
		fi
	}

	// Throw an assert violation if successful.
	assert(!success)
}
