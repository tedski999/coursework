#include "Common.hpp"
#include <opencv2/opencv.hpp>

void draw_text(cv::Mat &frame, std::string text, cv::Point position) {
	cv::putText(frame, text, position, cv::FONT_HERSHEY_COMPLEX, 0.5, {0, 0, 0}, 3);
	cv::putText(frame, text, position, cv::FONT_HERSHEY_COMPLEX, 0.5, {255, 255, 255});
}

cv::Mat join_images_h(cv::Mat &l, cv::Mat &r) {
	cv::Mat image = cv::Mat::zeros(std::max(l.rows, r.rows), l.cols + r.cols, l.type());
	l.copyTo(image(cv::Rect(0,      0, l.cols, l.rows)));
	r.copyTo(image(cv::Rect(l.cols, 0, r.cols, r.rows)));
	return image;
}

cv::Mat join_images_v(cv::Mat &t, cv::Mat &b) {
	cv::Mat image = cv::Mat::zeros(t.rows + b.rows, std::max(t.cols, b.cols), t.type());
	t.copyTo(image(cv::Rect(0, 0,      t.cols, t.rows)));
	b.copyTo(image(cv::Rect(0, t.rows, b.cols, b.rows)));
	return image;
}

void square_to_coords(int i, int *x, int *y) {
	*x = i / (BOARD_WIDTH / 2);
	*y = i % (BOARD_WIDTH / 2) * 2 + !(*x % 2);
}

int mod(int x, int m) {
	return (x % m + m) % m;
}

void parse_pdn(const std::string pdn, enum square_type man_type, enum square_type king_type, board board) {
	std::string piece;
	std::istringstream iss(pdn);
	while (std::getline(iss, piece, ',')) {
		// Is this piece a king?
		enum square_type piece_type = man_type;
		if (piece.at(0) == 'K') {
			piece.erase(0,1);
			piece_type = king_type;
		}
		// Mark piece location on board
		int location = std::stoi(piece);
		board[location - 1] = piece_type;
	}
}
