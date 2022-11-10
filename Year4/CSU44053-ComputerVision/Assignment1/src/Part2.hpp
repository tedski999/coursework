#ifndef PART2_HPP
#define PART2_HPP

#include "Common.hpp"
#include <opencv2/opencv.hpp>

struct part2_preprocessed_data {
	cv::Mat perspective_matrix;
	cv::MatND black_pieces_hist;
	cv::MatND white_pieces_hist;
	int *channel_numbers;
	int *number_bins;
	const float **channel_ranges;
};

void part2_classify_colours(struct part2_preprocessed_data *ppdata, cv::Mat image, board detected_board);
void run_part2();

#endif
