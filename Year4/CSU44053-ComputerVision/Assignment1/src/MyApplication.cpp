#include "MyApplication.hpp"
#include "Common.hpp"
#include "Part1.hpp"
#include "Part2.hpp"
#include "Part3.hpp"
#include "Part4.hpp"
#include "Part5.hpp"
#include <opencv2/opencv.hpp>

void MyApplication() {
	cv::Mat menu = cv::Mat::zeros(WINDOW_SIZE, CV_8UC3);
	draw_text(menu, "CSU44053 Computer Vision",        { 10, 20 });
	draw_text(menu, "Assignment 1 - Draughts",         { 10, 40 });
	draw_text(menu, "[1 - 5] Change Assignment Part",  { 10, 70 });
	draw_text(menu, "[ESCAPE] Quit Application",       { 10, 90 });
	draw_text(menu, "Part 1: Classify Pixels",         { 10, 120 });
	draw_text(menu, "Part 2: Classify Square Colours", { 10, 140 });
	draw_text(menu, "Part 3: Determine Game Moves",    { 10, 160 });
	draw_text(menu, "Part 4: Find Board Corners",      { 10, 180 });
	draw_text(menu, "Part 5: Classify Square Pieces",  { 10, 200 });
	draw_text(menu, "Ted Johnson - TCD 19335618",      { 10, 260 });

	cv::namedWindow(WINDOW);

	bool is_running = true;
	while (is_running && cv::getWindowProperty(WINDOW, cv::WND_PROP_VISIBLE)) {
		cv::imshow(WINDOW, menu);
		int key = cv::waitKeyEx(POLL_DELAY);

		try {
			switch (key) {
				case '1': run_part1(); break;
				case '2': run_part2(); break;
				case '3': run_part3(); break;
				case '4': run_part4(); break;
				case '5': run_part5(); break;
				case ESCAPE_KEY: is_running = false; break;
			}
		} catch (std::string msg) {
			std::cout << msg << std::endl;
		}
	}

	cv::destroyWindow(WINDOW);
}
