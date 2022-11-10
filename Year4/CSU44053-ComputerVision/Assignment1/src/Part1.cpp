#include "Part1.hpp"
#include "Common.hpp"
#include <opencv2/opencv.hpp>

struct preprocessed_data {
	cv::MatND black_pieces_hist;
	cv::MatND white_pieces_hist;
	cv::MatND black_squares_hist;
	cv::MatND white_squares_hist;
	int *channel_numbers;
	int *number_bins;
	const float **channel_ranges;
};

static void classify_pixels(struct preprocessed_data *ppdata, cv::Mat &image) {

	// Back projection with each histogram
	cv::Mat bp, wp, bs, ws;
	cv::cvtColor(image, image, cv::COLOR_BGR2HLS);
	cv::calcBackProject(&image, 1, ppdata->channel_numbers, ppdata->black_pieces_hist, bp, ppdata->channel_ranges, 255);
	cv::calcBackProject(&image, 1, ppdata->channel_numbers, ppdata->white_pieces_hist, wp, ppdata->channel_ranges, 255);
	cv::calcBackProject(&image, 1, ppdata->channel_numbers, ppdata->black_squares_hist, bs, ppdata->channel_ranges, 255);
	cv::calcBackProject(&image, 1, ppdata->channel_numbers, ppdata->white_squares_hist, ws, ppdata->channel_ranges, 255);

	// Colour each pixel depending on the highest probablity back projection for that pixel
	cv::Vec3b colours[4] = { {0,0,0}, {255,255,255}, {0,255,0}, {0,0,255} };
	for (int row = 0; row < image.rows; row++) {
		for (int col = 0; col < image.cols; col++) {
			uchar probablities[4] = { bp.at<uchar>(row, col), wp.at<uchar>(row, col), bs.at<uchar>(row, col), ws.at<uchar>(row, col) };
			int max_probablity_index = std::max_element(probablities, probablities + 4) - probablities;
			image.at<cv::Vec3b>(row, col) = colours[max_probablity_index];
		}
	}

	// Close any holes to reduce noise
	cv::morphologyEx(image, image, cv::MORPH_CLOSE, cv::getStructuringElement(cv::MORPH_RECT, {5,5}));
}

static void evaluate(struct preprocessed_data *ppdata) {

	// Initialise test
	cv::Mat image = cv::imread(GROUND_TRUTHS_MOVE0_FILE);
	cv::Mat ground_truth_image = cv::imread(GROUND_TRUTHS_MOVE0_PIXELS_FILE);
	if (image.empty() || ground_truth_image.empty()) {
		std::string error_msg = "Cannot open file(s):";
		if (image.empty()) error_msg += '\n' + std::string(GROUND_TRUTHS_MOVE0_FILE);
		if (ground_truth_image.empty()) error_msg += '\n' + std::string(GROUND_TRUTHS_MOVE0_PIXELS_FILE);
		throw error_msg;
	}

	// Compare our detector with ground truth
	classify_pixels(ppdata, image);
	cv::Mat diff;
	cv::absdiff(image, ground_truth_image, diff);

	// Compute score
	double score = 0;
	for (int row = 0; row < diff.rows; row++)
		for (int col = 0; col < diff.cols; col++)
			score += (diff.at<cv::Vec3b>(row, col) == cv::Vec3b::zeros());
	score /= diff.rows * diff.cols;

	// Output results
	draw_text(diff, std::to_string(score * 100) + "% pixels correct", { 20, 20 });
	draw_text(diff, "Black pixels were correctly identified", { 20, 40 });
	draw_text(diff, "[ESCAPE] Go Back", { 20, 70 });

	bool is_running = true;
	while (is_running && cv::getWindowProperty(WINDOW, cv::WND_PROP_VISIBLE)) {
		cv::imshow(WINDOW, diff);
		is_running = (cv::waitKeyEx(POLL_DELAY) != ESCAPE_KEY);
	}
}

void run_part1() {

	// Load required files
	cv::VideoCapture video(VIDEO_FILENAME);
	cv::Mat black_pieces_image = cv::imread(BLACK_PIECES_FILENAME, -1);
	cv::Mat white_pieces_image = cv::imread(WHITE_PIECES_FILENAME, -1);
	cv::Mat black_squares_image = cv::imread(BLACK_SQUARES_FILENAME, -1);
	cv::Mat white_squares_image = cv::imread(WHITE_SQUARES_FILENAME, -1);
	if (!video.isOpened() || black_pieces_image.empty() || white_pieces_image.empty() || black_squares_image.empty() || white_squares_image.empty()) {
		std::string error_msg = "Cannot open file(s):";
		if (!video.isOpened()) error_msg += '\n' + std::string(VIDEO_FILENAME);
		if (black_pieces_image.empty()) error_msg += '\n' + std::string(BLACK_PIECES_FILENAME);
		if (white_pieces_image.empty()) error_msg += '\n' + std::string(WHITE_PIECES_FILENAME);
		if (black_squares_image.empty()) error_msg += '\n' + std::string(BLACK_SQUARES_FILENAME);
		if (white_squares_image.empty()) error_msg += '\n' + std::string(WHITE_SQUARES_FILENAME);
		throw error_msg;
	}

	// Define histogram parameters
	int channel_numbers[] = { 0, 1, 2 };
	float ch_range[2] = { 0, 255 };
	const float *channel_ranges[3] = { ch_range, ch_range, ch_range };
	int number_bins[256];
	std::fill_n(number_bins, 256, 4);

	// Compute required histograms
	cv::MatND black_pieces_hist;
	cv::cvtColor(black_pieces_image, black_pieces_image, cv::COLOR_BGR2HLS);
	cv::calcHist(&black_pieces_image, 1, channel_numbers, cv::Mat(), black_pieces_hist, black_pieces_image.channels(), number_bins, channel_ranges);
	cv::normalize(black_pieces_hist, black_pieces_hist);
	cv::MatND white_pieces_hist;
	cv::cvtColor(white_pieces_image, white_pieces_image, cv::COLOR_BGR2HLS);
	cv::calcHist(&white_pieces_image, 1, channel_numbers, cv::Mat(), white_pieces_hist, white_pieces_image.channels(), number_bins, channel_ranges);
	cv::normalize(white_pieces_hist, white_pieces_hist);
	cv::MatND black_squares_hist;
	cv::cvtColor(black_squares_image, black_squares_image, cv::COLOR_BGR2HLS);
	cv::calcHist(&black_squares_image, 1, channel_numbers, cv::Mat(), black_squares_hist, black_squares_image.channels(), number_bins, channel_ranges);
	cv::normalize(black_squares_hist, black_squares_hist);
	cv::MatND white_squares_hist;
	cv::cvtColor(white_squares_image, white_squares_image, cv::COLOR_BGR2HLS);
	cv::calcHist(&white_squares_image, 1, channel_numbers, cv::Mat(), white_squares_hist, white_squares_image.channels(), number_bins, channel_ranges);
	cv::normalize(white_squares_hist, white_squares_hist);

	struct preprocessed_data ppdata = {
		.black_pieces_hist = black_pieces_hist,
		.white_pieces_hist = white_pieces_hist,
		.black_squares_hist = black_squares_hist,
		.white_squares_hist = white_squares_hist,
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

		// Process the currently selected frame
		cv::Mat image, classified_image;
		video.set(cv::CAP_PROP_POS_FRAMES, frame_index);
		video.read(image);
		classified_image = image.clone();
		classify_pixels(&ppdata, classified_image);

		cv::Mat display = join_images_h(image, classified_image);
		if (draw_ui) {
			draw_text(display, "Part 1: Classify Pixels", { 10, 20 });
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
