#include "Part2.hpp"
#include "Common.hpp"
#include <opencv2/opencv.hpp>

static void evaluate(struct part2_preprocessed_data *ppdata) {

	struct test {
		std::string name;
		cv::Mat image;
		board detected_board;
		board ground_truth_board;
		cv::Mat confusion_matrix;
		double score;
	};

	// Initialse tests
	int tests_len = sizeof GROUND_TRUTHS_BOARD / sizeof *GROUND_TRUTHS_BOARD;
	struct test tests[tests_len];
	for (int i = 0; i < tests_len; i++) {
		tests[i].name = GROUND_TRUTHS_BOARD[i][0];
		tests[i].image = cv::imread(tests[i].name);
		if (tests[i].image.empty())
			throw "Cannot open file: " + tests[i].name;
		std::fill(std::begin(tests[i].ground_truth_board), std::end(tests[i].ground_truth_board), EMPTY_SQUARE);
		parse_pdn(GROUND_TRUTHS_BOARD[i][1], WHITE_MAN, WHITE_MAN, tests[i].ground_truth_board);
		parse_pdn(GROUND_TRUTHS_BOARD[i][2], BLACK_MAN, BLACK_MAN, tests[i].ground_truth_board);
		tests[i].confusion_matrix = cv::Mat::zeros(3, 3, CV_32S);
		tests[i].score = 0;
	}

	// Run all the tests
	for (int i = 0; i < tests_len; i++) {
		// Run our detector
		cv::Mat image = cv::Mat::zeros(FRAME_WIDTH, FRAME_WIDTH, CV_8UC3);
		cv::warpPerspective(tests[i].image, image, ppdata->perspective_matrix, image.size());
		part2_classify_colours(ppdata, image, tests[i].detected_board);
		// Add to results
		for (int j = 0; j < BOARD_SIZE; j++)
			tests[i].confusion_matrix.at<int>(tests[i].detected_board[j], tests[i].ground_truth_board[j]) += 1;
		for (int j = 0; j < tests[i].confusion_matrix.rows; j++)
			tests[i].score += tests[i].confusion_matrix.at<int>(j, j);
		tests[i].score = tests[i].score / (int) BOARD_SIZE;
	}

	// Compute total results
	cv::Mat total_confusion_matrix = cv::Mat::zeros(3, 3, CV_32S);
	for (int i = 0; i < tests_len; i++)
		total_confusion_matrix += tests[i].confusion_matrix;
	double total_score = 0;
	for (int i = 0; i < total_confusion_matrix.rows; i++)
		total_score += total_confusion_matrix.at<int>(i, i);
	total_score = total_score / (tests_len * (int) BOARD_SIZE);

	// Output results
	for (int i = 0; i < tests_len; i++) {
		std::cout << tests[i].name << " - " << 100 * tests[i].score << "% correct" << std::endl;
		std::cout << tests[i].confusion_matrix << std::endl;
		std::cout << std::endl;
	}
	std::cout << "Total - " << 100 * total_score << "% correct" << std::endl;
	std::cout << total_confusion_matrix << std::endl;
}


void part2_classify_colours(struct part2_preprocessed_data *ppdata, cv::Mat image, board detected_board) {

	// Compare the histogram of each square on the board with the histograms of the pieces
	std::vector<float> scores;
	for (int square = 0; square < BOARD_SIZE; square++) {
		int x, y;
		square_to_coords(square, &x, &y);
		cv::Mat square_image = image({x * SQUARE_WIDTH, y * SQUARE_WIDTH, SQUARE_WIDTH, SQUARE_WIDTH});
		cv::MatND square_hist;
		cv::calcHist(&square_image, 1, ppdata->channel_numbers, cv::Mat(), square_hist, square_image.channels(), ppdata->number_bins, ppdata->channel_ranges);
		cv::normalize(square_hist, square_hist);
		float black_score = cv::compareHist(square_hist, ppdata->black_pieces_hist, cv::HISTCMP_BHATTACHARYYA);
		float white_score = cv::compareHist(square_hist, ppdata->white_pieces_hist, cv::HISTCMP_BHATTACHARYYA);
		scores.push_back(black_score - white_score);
	}

	// Use k-means to determine the threshold for a squares score to be considered empty, black, or white
	std::vector<int> labels;
	std::vector<float> centers;
	cv::Mat scores_mat(scores.size(), 1, CV_32FC1, &scores[0]);
	cv::kmeans(scores_mat, 3, labels, cv::TermCriteria(cv::TermCriteria::MAX_ITER+cv::TermCriteria::EPS, 50, 0.00001), 100, 0, centers);

	// We need to figure out what each label actually means and store the results in an array of square types
	enum square_type label_map[3] = { EMPTY_SQUARE, EMPTY_SQUARE, EMPTY_SQUARE };
	auto it = std::minmax_element(centers.begin(), centers.end());
	label_map[std::distance(centers.begin(), it.first)] = BLACK_MAN;
	label_map[std::distance(centers.begin(), it.second)] = WHITE_MAN;
	for (int square = 0; square < BOARD_SIZE; square++) {
		detected_board[square] = label_map[labels[square]];
	}
}

void run_part2() {

	// Load required files
	cv::VideoCapture video(VIDEO_FILENAME);
	cv::Mat black_pieces_image = cv::imread(BLACK_PIECES_FILENAME, -1);
	cv::Mat white_pieces_image = cv::imread(WHITE_PIECES_FILENAME, -1);
	if (!video.isOpened() || black_pieces_image.empty() || white_pieces_image.empty()) {
		std::string error_msg = "Cannot open file(s):";
		if (!video.isOpened()) error_msg += '\n' + std::string(VIDEO_FILENAME);
		if (black_pieces_image.empty()) error_msg += '\n' + std::string(BLACK_PIECES_FILENAME);
		if (white_pieces_image.empty()) error_msg += '\n' + std::string(WHITE_PIECES_FILENAME);
		throw error_msg;
	}

	// Generate board perspective matrix from provided corner points
	cv::Point2f board_corners[4] = { { 114, 17 }, { 53,  245 }, { 355, 20 }, { 433, 241 } };
	cv::Point2f perspective_corners[4] = { { 0, 0 }, { 0, FRAME_WIDTH }, { FRAME_WIDTH, 0 }, { FRAME_WIDTH, FRAME_WIDTH } };
	cv::Mat perspective_matrix = cv::getPerspectiveTransform(board_corners, perspective_corners);

	// Define histogram parameters
	int channel_numbers[] = { 0, 1, 2 };
	float ch_range[2] = { 0, 255 };
	const float *channel_ranges[3] = { ch_range, ch_range, ch_range };
	int number_bins[256];
	std::fill_n(number_bins, 256, 3);

	// Compute required histograms
	cv::MatND black_pieces_hist;
	cv::calcHist(&black_pieces_image, 1, channel_numbers, cv::Mat(), black_pieces_hist, black_pieces_image.channels(), number_bins, channel_ranges);
	cv::normalize(black_pieces_hist, black_pieces_hist);
	cv::MatND white_pieces_hist;
	cv::calcHist(&white_pieces_image, 1, channel_numbers, cv::Mat(), white_pieces_hist, white_pieces_image.channels(), number_bins, channel_ranges);
	cv::normalize(white_pieces_hist, white_pieces_hist);

	struct part2_preprocessed_data ppdata = {
		.perspective_matrix = perspective_matrix,
		.black_pieces_hist = black_pieces_hist,
		.white_pieces_hist = white_pieces_hist,
		.channel_numbers = channel_numbers,
		.number_bins = number_bins,
		.channel_ranges = channel_ranges
	};

	// Process user-selected frames from video
	int frame_index = 0;
	bool is_running = true;
	bool is_paused = true;
	bool draw_ui = true;
	while (is_running && cv::getWindowProperty(WINDOW, cv::WND_PROP_VISIBLE)) {

		// Wrap selection to video length
		frame_index = mod(frame_index, video.get(cv::CAP_PROP_FRAME_COUNT));

		// Read the selected frame
		cv::Mat image;
		board detected_board;
		video.set(cv::CAP_PROP_POS_FRAMES, frame_index);
		video.read(image);

		// Process the selected frame
		cv::Mat perspective_image = cv::Mat::zeros(FRAME_WIDTH, FRAME_WIDTH, CV_8UC3);
		cv::warpPerspective(image, perspective_image, ppdata.perspective_matrix, perspective_image.size());
		part2_classify_colours(&ppdata, perspective_image, detected_board);

		// Draw detected board
		cv::Mat board_image(cv::Size(200, 200), CV_8UC3);
		board_image.setTo(cv::Scalar(255,255,255));
		for (int i = 0; i < BOARD_SIZE; i++) {
			int x, y;
			square_to_coords(i, &x, &y);
			double size = (double) board_image.rows / BOARD_WIDTH;
			cv::Rect square(x * size, y * size, size, size);
			cv::rectangle(board_image, square, {0,0,0}, -1);
			if (detected_board[i] != EMPTY_SQUARE) {
				cv::Point center((x+0.5) * size, (y+0.5) * size);
				int thickness = (detected_board[i] == WHITE_MAN) ? -1 : 1;
				cv::circle(board_image, center, size / 4, {255,255,255}, thickness);
			}
		}

		cv::Mat display = join_images_h(image, board_image);
		if (draw_ui) {
			draw_text(display, "Part 2: Classify Square Colours", { 10, 20 });
			draw_text(display, "[u] Toggle UI", { 10, 50 });
			draw_text(display, "[e] Run Evaluation", { 10, 70 });
			draw_text(display, "[hjkl] Navigate Moves", { 10, 90 });
			draw_text(display, "[SPACE] Toggle Playback", { 10, 110 });
			draw_text(display, "[ESCAPE] Back To Menu", { 10, 130 });
		}

		cv::imshow(WINDOW, display);
		switch (cv::waitKeyEx(is_paused ? POLL_DELAY : 1)) {
			case 'u': draw_ui = !draw_ui; break;
			case 'e': evaluate(&ppdata); break;
			case ' ': is_paused = !is_paused; break;
			case 'h': frame_index -= 1;  is_paused = true; break;
			case 'l': frame_index += 1;  is_paused = true; break;
			case 'j': frame_index -= 10; is_paused = true; break;
			case 'k': frame_index += 10; is_paused = true; break;
			case ESCAPE_KEY: is_running = false; break;
		}

		if (!is_paused) {
			frame_index += 1;
		}
	}
}
