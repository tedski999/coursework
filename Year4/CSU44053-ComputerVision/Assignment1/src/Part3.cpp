#include "Part3.hpp"
#include "Part2.hpp"
#include "Common.hpp"
#include <opencv2/opencv.hpp>

void run_part3() {

	// Show text to the user to they know somethings happening
	cv::Mat loading_image = cv::Mat::zeros(WINDOW_SIZE, CV_8UC3);
	draw_text(loading_image, "Processing video...", {20,20});
	cv::imshow(WINDOW, loading_image);
	cv::waitKeyEx(1);

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

	// Find all frames without much movement in them
	cv::Mat image;
	bool was_movement_detected = true;
	cv::Ptr<cv::BackgroundSubtractorMOG2> gmm = cv::createBackgroundSubtractorMOG2(3);
	std::vector<cv::Mat> frames;
	while (video.read(image)) {
		// Fix image perspective
		cv::Mat perspective_image = cv::Mat::zeros(FRAME_WIDTH, FRAME_WIDTH, CV_8UC3);
		cv::warpPerspective(image, perspective_image, perspective_matrix, perspective_image.size());
		// Update the GMM and get the median background image
		cv::Mat foreground_mask, background_image;
		gmm->apply(perspective_image, foreground_mask);
		gmm->getBackgroundImage(background_image);
		// Detect movement by taking the difference between this frame and the median background image
		cv::Mat diff;
		cv::absdiff(background_image, perspective_image, diff);
		cv::threshold(diff, diff, 20, 255, cv::THRESH_BINARY);
		cv::cvtColor(diff, diff, cv::COLOR_BGR2GRAY);
		bool movement_detected = (cv::countNonZero(diff) != 0);
		// If this frame have no movement in it but the last frame did, consider this frame for later processing
		if (!movement_detected && was_movement_detected)
			frames.push_back(perspective_image);
		was_movement_detected = movement_detected;
	}

	struct move {
		cv::Mat frame;
		int from_square;
		int to_square;
	};

	// Determine, if any, which square moved to which other square between every frame
	std::vector<struct move> moves;
	for (int i = 0; i < frames.size() - 1; i++) {
		board from_board, to_board;
		int from_square = 0, to_square = 0;

		part2_classify_colours(&ppdata, frames[i], from_board);
		part2_classify_colours(&ppdata, frames[i+1], to_board);

		// Find where, if any, a piece has moved to
		for (int i = 0; i < BOARD_SIZE; i++)
			if (from_board[i] == EMPTY_SQUARE && to_board[i] != EMPTY_SQUARE)
				to_square = i;
		if (to_square == 0)
			continue;

		enum square_type piece_type = to_board[to_square];

		// Find where, if any, the piece came from
		for (int i = 0; i < BOARD_SIZE; i++)
			if (from_board[i] == piece_type && to_board[i] == EMPTY_SQUARE)
				from_square = i;
		if (from_square == 0)
			continue;

		// Record the move
		moves.push_back((struct move) {
			.frame = frames[i],
			.from_square = from_square,
			.to_square = to_square
		});
	}

	// Process user-selected frames from video
	int move_index = 0;
	bool is_running = true;
	bool is_paused = true;
	bool draw_ui = true;
	while (is_running && cv::getWindowProperty(WINDOW, cv::WND_PROP_VISIBLE)) {

		// Wrap selection to video length
		move_index = mod(move_index, moves.size());

		// Draw the currently selected frame
		int from_x, from_y, to_x, to_y;
		square_to_coords(moves[move_index].from_square, &from_x, &from_y);
		square_to_coords(moves[move_index].to_square, &to_x, &to_y);
		cv::Point2d from = cv::Point2d(from_x + 0.5, from_y + 0.5) * FRAME_WIDTH / BOARD_WIDTH;
		cv::Point2d to = cv::Point2d(to_x + 0.5, to_y + 0.5) * FRAME_WIDTH / BOARD_WIDTH;
		cv::Mat display = moves[move_index].frame.clone();
		cv::arrowedLine(display, from, to, {255,255,255}, 2);

		if (draw_ui) {
			draw_text(display, "Part 3: Determine Game Moves", { 10, 20 });
			draw_text(display, "[u] Toggle UI", { 10, 50 });
			draw_text(display, "[hjkl] Navigate Moves", { 10, 70 });
			draw_text(display, "[SPACE] Toggle Playback", { 10, 90 });
			draw_text(display, "[ESCAPE] Back To Menu", { 10, 110 });
		}

		cv::imshow(WINDOW, display);
		switch (cv::waitKeyEx(is_paused ? POLL_DELAY : 1)) {
			case 'u': draw_ui = !draw_ui; break;
			case ' ': is_paused = !is_paused; break;
			case 'h': move_index -= 1;  is_paused = true; break;
			case 'l': move_index += 1;  is_paused = true; break;
			case 'j': move_index -= 10; is_paused = true; break;
			case 'k': move_index += 10; is_paused = true; break;
			case ESCAPE_KEY: is_running = false; break;
		}

		if (!is_paused) {
			move_index += 1;
		}
	}
}
