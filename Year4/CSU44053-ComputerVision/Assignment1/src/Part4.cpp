#include "Part4.hpp"
#include "Common.hpp"
#include <opencv2/opencv.hpp>

#define LOW_THRESH 100
#define HIGH_THRESH 200

typedef std::array<cv::Point2d, 4> corners;

static corners hough(cv::Mat image) {
	cv::Mat grey;
	cv::cvtColor(image, grey, cv::COLOR_BGR2GRAY);
	cv::Mat edges;
	cv::Canny(grey, edges, 100, 200);

	// int x = 100, y = 100;
	// while (true) {
	// cv::Canny(grey, edges, x, y);

	cv::Mat hough_image = image.clone();
	std::vector<cv::Vec2f> hough_lines;
	cv::HoughLines(edges, hough_lines, 1, M_PI / 180, 150);
	for (cv::Vec2f line : hough_lines) {
		float a = cos(line[1]);
		float b = sin(line[1]);
		float x0 = a * line[0];
		float y0 = b * line[0];

		int x1 = x0 - 999 * b;
		int y1 = y0 + 999 * a;
		int x2 = x0 + 999 * b;
		int y2 = y0 - 999 * a;

		cv::line(hough_image, {x1, y1}, {x2, y2}, {0,0,255}, 2);
	}

	// cv::imshow("Hough line transform", hough_image);
	// draw_text(edges, std::to_string(x) + " " + std::to_string(y), { 10, 10 });
	// cv::imshow("canny", edges);
	// int k = cv::waitKeyEx();
	// if (k == 'a') x -= 10;
	// if (k == 'd') x += 10;
	// if (k == 'w') y += 10;
	// if (k == 's') y -= 10;
	// }

	return {};
}

static corners contour(cv::Mat image) {
	cv::Mat grey;
	cv::cvtColor(image, grey, cv::COLOR_BGR2GRAY);
	cv::Mat edges;


	cv::Canny(grey, edges, 100, 400);

	// int x = 100, y = 100;
	// while (true) {
	// cv::Canny(grey, edges, x, y);

	cv::Mat contour_image = image.clone();
	std::vector<std::vector<cv::Point>> contours;
	std::vector<cv::Vec4i> hierarchy;
	cv::findContours(edges, contours, hierarchy, cv::RETR_LIST, cv::CHAIN_APPROX_SIMPLE);
	for (int i = 0; i < contours.size(); i++) {
		cv::approxPolyDP(cv::Mat(contours[i]), contours[i], 5, false);
	}

	srand(contours.size());
	for (int i = 0; i < contours.size(); i++) {
		cv::Scalar colour(rand()%0xff, rand()%0xff, rand()%0xff);
		cv::drawContours(contour_image, contours, i, colour, 1, 8, hierarchy);
	}

	// cv::imshow("Contour following and straight line segmentation", contour_image);
	// draw_text(edges, std::to_string(x) + " " + std::to_string(y), { 10, 10 });
	// cv::imshow("canny", edges);
	// int k = cv::waitKeyEx();
	// if (k == 'a') x -= 10;
	// if (k == 'd') x += 10;
	// if (k == 'w') y += 10;
	// if (k == 's') y -= 10;
	// }

	return {};
}

static corners findChessboardCorners(cv::Mat image) {
	// Clean up the image for better recognition
	cv::Mat grey;
	cv::cvtColor(image, grey, cv::COLOR_BGR2GRAY);
	cv::inRange(grey, cv::Scalar(LOW_THRESH), cv::Scalar(HIGH_THRESH), grey);
	cv::morphologyEx(grey, grey, cv::MORPH_OPEN, cv::getStructuringElement(cv::MORPH_RECT,{5,5}));
	cv::bitwise_not(grey, grey);
	// Use findChessboardCorners to find all inner corners on the board
	std::vector<cv::Point2f> corners;
	cv::Size size = {BOARD_WIDTH-1, BOARD_WIDTH-1};
	if (!cv::findChessboardCorners(grey, size, corners)) {
		return {};
	}
	// Now we need to find the four outer corners of the board
	cv::Point2d top_left     = corners[          1 * size.width - size.width];
	cv::Point2d top_right    = corners[          1 * size.width - 1];
	cv::Point2d bottom_left  = corners[size.height * size.width - size.width];
	cv::Point2d bottom_right = corners[size.height * size.width - 1];
	// Generate an inverse perspective matrix to map points from a square proportional to the board size back to the original image
	cv::Point2f board_corners[4] = { top_left, bottom_left, top_right, bottom_right };
	cv::Point2f inverse_perspective_corners[4] = { { 1, 1 }, { 1, (float) size.height }, { (float) size.width, 1 }, { (float) size.width, (float) size.height } };
	cv::Mat inverse_perspective_matrix = cv::getPerspectiveTransform(inverse_perspective_corners, board_corners);
	// Map the four outer-most inner corners to the four outer corners using the inverse perspective matrix and normalising the z-axis
	cv::Vec3d tl = cv::Mat(inverse_perspective_matrix * cv::Mat((cv::Vec3d) {0,           0,           1})).col(0);
	cv::Vec3d bl = cv::Mat(inverse_perspective_matrix * cv::Mat((cv::Vec3d) {0,           BOARD_WIDTH, 1})).col(0);
	cv::Vec3d tr = cv::Mat(inverse_perspective_matrix * cv::Mat((cv::Vec3d) {BOARD_WIDTH, 0,           1})).col(0);
	cv::Vec3d br = cv::Mat(inverse_perspective_matrix * cv::Mat((cv::Vec3d) {BOARD_WIDTH, BOARD_WIDTH, 1})).col(0);
	return {
		cv::Point2d(tl[0] / tl[2], tl[1] / tl[2]),
		cv::Point2d(tr[0] / tr[2], tr[1] / tr[2]),
		cv::Point2d(bl[0] / bl[2], bl[1] / bl[2]),
		cv::Point2d(br[0] / br[2], br[1] / br[2])
	};
}

static void evaluate() {

	corners ground_truth_board_corners = {
		cv::Point2d(114, 17),
		cv::Point2d(355, 20),
		cv::Point2d(53, 245),
		cv::Point2d(433, 241)
	};

	struct test {
		std::string name;
		cv::Mat image;
		corners hough_corners;
		corners contours_corners;
		corners findChessboardCorners_corners;
		double hough_penalty;
		double contours_penalty;
		double findChessboardCorners_penalty;
	};

	// Initialise tests
	int tests_len = sizeof GROUND_TRUTHS_BOARD / sizeof *GROUND_TRUTHS_BOARD;
	struct test tests[tests_len];
	for (int i = 0; i < tests_len; i++) {
		tests[i].name = GROUND_TRUTHS_BOARD[i][0];
		tests[i].image = cv::imread(tests[i].name);
		if (tests[i].image.empty())
			throw "Cannot open file: " + tests[i].name;
	}

	// Run all the tests
	for (int i = 0; i < tests_len; i++) {
		// Run corner detectors
		tests[i].hough_corners = hough(tests[i].image);
		tests[i].contours_corners = contour(tests[i].image);
		tests[i].findChessboardCorners_corners = findChessboardCorners(tests[i].image);
		// Find distance from ground truth for each result
		tests[i].hough_penalty =
			cv::norm(tests[i].hough_corners[0] - ground_truth_board_corners[0]) +
			cv::norm(tests[i].hough_corners[1] - ground_truth_board_corners[1]) +
			cv::norm(tests[i].hough_corners[2] - ground_truth_board_corners[2]) +
			cv::norm(tests[i].hough_corners[3] - ground_truth_board_corners[3]);
		tests[i].contours_penalty =
			cv::norm(tests[i].contours_corners[0] - ground_truth_board_corners[0]) +
			cv::norm(tests[i].contours_corners[1] - ground_truth_board_corners[1]) +
			cv::norm(tests[i].contours_corners[2] - ground_truth_board_corners[2]) +
			cv::norm(tests[i].contours_corners[3] - ground_truth_board_corners[3]);
		tests[i].findChessboardCorners_penalty =
			cv::norm(tests[i].findChessboardCorners_corners[0] - ground_truth_board_corners[0]) +
			cv::norm(tests[i].findChessboardCorners_corners[1] - ground_truth_board_corners[1]) +
			cv::norm(tests[i].findChessboardCorners_corners[2] - ground_truth_board_corners[2]) +
			cv::norm(tests[i].findChessboardCorners_corners[3] - ground_truth_board_corners[3]);
	}

	// Compute total results
	double total_hough_penalty = 0;
	double total_contours_penalty = 0;
	double total_findChessboardCorners_penalty = 0;
	for (int i = 0; i < tests_len; i++) {
		total_hough_penalty += tests[i].hough_penalty;
		total_contours_penalty += tests[i].contours_penalty;
		total_findChessboardCorners_penalty += tests[i].findChessboardCorners_penalty;
	}
	total_hough_penalty /= tests_len;
	total_contours_penalty /= tests_len;
	total_findChessboardCorners_penalty /= tests_len;

	// Output results
	for (int i = 0; i < tests_len; i++) {
		std::cout << tests[i].name << std::endl;
		std::cout << "hough: " << tests[i].hough_penalty << " off" << std::endl;
		std::cout << "contours: " << tests[i].contours_penalty << " off" << std::endl;
		std::cout << "findChessboardCorners: " << tests[i].findChessboardCorners_penalty << " off" << std::endl;
		std::cout << std::endl;
	}
	std::cout << "Averages" << std::endl;
	std::cout << "hough: " << total_hough_penalty << " off" << std::endl;
	std::cout << "contours: " << total_contours_penalty<< " off"  << std::endl;
	std::cout << "findChessboardCorners: " << total_findChessboardCorners_penalty << " off" << std::endl;
}

void run_part4() {

	// Load required files
	cv::VideoCapture video(VIDEO_FILENAME);
	if (!video.isOpened()) {
		std::string error_msg = "Cannot open file(s):";
		if (!video.isOpened()) error_msg += '\n' + std::string(VIDEO_FILENAME);
		throw error_msg;
	}

	// Process user-selected frames from video
	int frame_index = 0;
	bool is_running = true;
	bool is_paused = true;
	bool draw_ui = true;
	while (is_running && cv::getWindowProperty(WINDOW, cv::WND_PROP_VISIBLE)) {

		// Wrap selection to video length
		frame_index = mod(frame_index, video.get(cv::CAP_PROP_FRAME_COUNT));

		// Process the currently selected frame
		cv::Mat image;
		video.set(cv::CAP_PROP_POS_FRAMES, frame_index);
		video.read(image);
		corners hough_corners = hough(image.clone());
		corners contour_corners = contour(image.clone());
		corners findChessboardCorners_corners = findChessboardCorners(image.clone());

		// Draw detected corners
		cv::Mat display = image.clone();
		for (int i = 0; i < BOARD_SIZE; i++) {
			/* These algorithms don't work for this purpose so their outputs don't need to be drawn
			for (cv::Point2d corner : hough_corners)
				cv::circle(display, corner, 10, cv::Scalar(255,0,0), 2);
			for (cv::Point2d corner : contour_corners)
				cv::circle(display, corner, 10, cv::Scalar(0,255,0), 2);
			*/
			for (cv::Point2d corner : findChessboardCorners_corners)
				cv::circle(display, corner, 10, cv::Scalar(0,0,255), 2);
		}

		if (draw_ui) {
			draw_text(display, "Part 4: Find Board Corners", { 10, 20 });
			draw_text(display, "[u] Toggle UI", { 10, 50 });
			draw_text(display, "[e] Run Evaluation", { 10, 70 });
			draw_text(display, "[hjkl] Navigate Moves", { 10, 90 });
			draw_text(display, "[SPACE] Toggle Playback", { 10, 110 });
			draw_text(display, "[ESCAPE] Back To Menu", { 10, 130 });
		}

		cv::imshow(WINDOW, display);
		switch (cv::waitKeyEx(is_paused ? POLL_DELAY : 1)) {
			case 'u': draw_ui = !draw_ui; break;
			case 'e': evaluate(); break;
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
