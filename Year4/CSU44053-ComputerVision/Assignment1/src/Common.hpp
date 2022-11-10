#ifndef COMMON_HPP
#define COMMON_HPP

#include <opencv2/opencv.hpp>

#define WINDOW "Draughts"
#define WINDOW_SIZE cv::Size(480, 270)
#define POLL_DELAY 1000
#define ESCAPE_KEY 27

#define BOARD_WIDTH 8
#define BOARD_SIZE (BOARD_WIDTH * BOARD_WIDTH / 2)

#define SQUARE_WIDTH 50
#define FRAME_WIDTH (BOARD_WIDTH * SQUARE_WIDTH)

#define VIDEO_FILENAME "./res/DraughtsGame1.avi"
#define BOARD_FILENAME "./res/DraughtsGame1EmptyBoard.jpg"
#define BLACK_PIECES_FILENAME "./res/DraughtsGame1BlackPieces.jpg"
#define WHITE_PIECES_FILENAME "./res/DraughtsGame1WhitePieces.jpg"
#define BLACK_SQUARES_FILENAME "./res/DraughtsGame1BlackSquares.jpg"
#define WHITE_SQUARES_FILENAME "./res/DraughtsGame1WhiteSquares.jpg"

enum square_type { EMPTY_SQUARE, BLACK_MAN, WHITE_MAN, BLACK_KING, WHITE_KING };
typedef enum square_type board[BOARD_SIZE];

void draw_text(cv::Mat &frame, std::string text, cv::Point position);
cv::Mat join_images_h(cv::Mat &l, cv::Mat &r);
cv::Mat join_images_v(cv::Mat &t, cv::Mat &b);
void square_to_coords(int i, int *x, int *y);
int mod(int x, int m);
void parse_pdn(const std::string pdn, enum square_type man_type, enum square_type king_type, board board);

static const std::string GROUND_TRUTHS_MOVE0_FILE = "res/DraughtsGame1Move0.jpg";
static const std::string GROUND_TRUTHS_MOVE0_PIXELS_FILE = "res/DraughtsGame1Move0GroundTruth.png";

static const int GROUND_TRUTHS_MOVES[][3] = {
	{9, 13}, {24,20}, {6,  9},  {22, 17}, {13,22}, {26,17}, {9, 13}, {30,26}, {13,22}, {25,18},
	{12,16}, {18,14}, {10, 17}, {21, 14}, {2, 6},  {26,22}, {6, 9},  {22,18}, {11,15}, {18,2},
	{9, 18}, {23,14}, {3,  7},  {20, 11}, {7, 16}, {2, 7},  {8, 11}, {27,24}, {1, 6},  {7, 2},
	{6, 9},  {14,10}, {9,  14}, {10, 7},  {14,17}, {7, 3},  {11,15}, {24,20}, {16,19}, {3, 7},
	{15,18}, {7, 10}, {18, 22}, {10, 14}, {17,21}, {14,17}, {21,25}, {17,26}, {25,30}, {31,27},
	{30,23}, {27,18}, {19, 23}, {18, 15}, {23,26}, {15,11}, {26,31}, {32,27}, {31,24}, {28,19},
	{5, 9},  {29,25}, {9,  14}, {25, 22}, {14,18}, {22,15}, {4, 8},  {11,4}
};

static const std::string GROUND_TRUTHS_BOARD[][3] = {
	{"res/DraughtsGame1Move0.jpg",  "1,2,3,4,5,6,7,8,9,10,11,12",  "21,22,23,24,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move1.jpg",  "1,2,3,4,5,6,7,8,10,11,12,13", "21,22,23,24,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move2.jpg",  "1,2,3,4,5,6,7,8,10,11,12,13", "20,21,22,23,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move3.jpg",  "1,2,3,4,5,7,8,9,10,11,12,13", "20,21,22,23,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move4.jpg",  "1,2,3,4,5,7,8,9,10,11,12,13", "17,20,21,23,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move5.jpg",  "1,2,3,4,5,7,8,9,10,11,12,22", "20,21,23,25,26,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move6.jpg",  "1,2,3,4,5,7,8,9,10,11,12",    "17,20,21,23,25,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move7.jpg",  "1,2,3,4,5,7,8,10,11,12,13",   "17,20,21,23,25,27,28,29,30,31,32"},
	{"res/DraughtsGame1Move8.jpg",  "1,2,3,4,5,7,8,10,11,12,13",   "17,20,21,23,25,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move9.jpg",  "1,2,3,4,5,7,8,10,11,12,22",   "20,21,23,25,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move10.jpg", "1,2,3,4,5,7,8,10,11,12",      "18,20,21,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move11.jpg", "1,2,3,4,5,7,8,10,11,16",      "18,20,21,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move12.jpg", "1,2,3,4,5,7,8,10,11,16",      "14,20,21,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move13.jpg", "1,2,3,4,5,7,8,11,16,17",      "20,21,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move14.jpg", "1,2,3,4,5,7,8,11,16",         "14,20,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move15.jpg", "1,3,4,5,6,7,8,11,16",         "14,20,23,26,27,28,29,31,32"},
	{"res/DraughtsGame1Move16.jpg", "1,3,4,5,6,7,8,11,16",         "14,20,22,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move17.jpg", "1,3,4,5,7,8,9,11,16",         "14,20,22,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move18.jpg", "1,3,4,5,7,8,9,11,16",         "14,18,20,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move19.jpg", "1,3,4,5,7,8,9,15,16",         "14,18,20,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move20.jpg", "1,3,4,5,8,9,16",              "K2,14,20,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move21.jpg", "1,3,4,5,8,16,18",             "K2,20,23,27,28,29,31,32"},
	{"res/DraughtsGame1Move22.jpg", "1,3,4,5,8,16",                "K2,14,20,27,28,29,31,32"},
	{"res/DraughtsGame1Move23.jpg", "1,4,5,7,8,16",                "K2,14,20,27,28,29,31,32"},
	{"res/DraughtsGame1Move24.jpg", "1,4,5,7,8",                   "K2,11,14,27,28,29,31,32"},
	{"res/DraughtsGame1Move25.jpg", "1,4,5,8,16",                  "K2,14,27,28,29,31,32"},
	{"res/DraughtsGame1Move26.jpg", "1,4,5,8,16",                  "K7,14,27,28,29,31,32"},
	{"res/DraughtsGame1Move27.jpg", "1,4,5,11,16",                 "K7,14,27,28,29,31,32"},
	{"res/DraughtsGame1Move28.jpg", "1,4,5,11,16",                 "K7,14,24,28,29,31,32"},
	{"res/DraughtsGame1Move29.jpg", "4,5,6,11,16",                 "K7,14,24,28,29,31,32"},
	{"res/DraughtsGame1Move30.jpg", "4,5,6,11,16",                 "K2,14,24,28,29,31,32"},
	{"res/DraughtsGame1Move31.jpg", "4,5,9,11,16",                 "K2,14,24,28,29,31,32"},
	{"res/DraughtsGame1Move32.jpg", "4,5,9,11,16",                 "K2,10,24,28,29,31,32"},
	{"res/DraughtsGame1Move33.jpg", "4,5,11,14,16",                "K2,10,24,28,29,31,32"},
	{"res/DraughtsGame1Move34.jpg", "4,5,11,14,16",                "K2,7,24,28,29,31,32"},
	{"res/DraughtsGame1Move35.jpg", "4,5,11,16,17",                "K2,7,24,28,29,31,32"},
	{"res/DraughtsGame1Move36.jpg", "4,5,11,16,17",                "K2,K3,24,28,29,31,32"},
	{"res/DraughtsGame1Move37.jpg", "4,5,15,16,17",                "K2,K3,24,28,29,31,32"},
	{"res/DraughtsGame1Move38.jpg", "4,5,15,16,17",                "K2,K3,20,28,29,31,32"},
	{"res/DraughtsGame1Move39.jpg", "4,5,15,17,19",                "K2,K3,20,28,29,31,32"},
	{"res/DraughtsGame1Move40.jpg", "4,5,15,17,19",                "K2,K7,20,28,29,31,32"},
	{"res/DraughtsGame1Move41.jpg", "4,5,17,18,19",                "K2,K7,20,28,29,31,32"},
	{"res/DraughtsGame1Move42.jpg", "4,5,17,18,19",                "K2,K10,20,28,29,31,32"},
	{"res/DraughtsGame1Move43.jpg", "4,5,17,19,22",                "K2,K10,20,28,29,31,32"},
	{"res/DraughtsGame1Move44.jpg", "4,5,17,19,22",                "K2,K14,20,28,29,31,32"},
	{"res/DraughtsGame1Move45.jpg", "4,5,19,21,22",                "K2,K14,20,28,29,31,32"},
	{"res/DraughtsGame1Move46.jpg", "4,5,19,21,22",                "K2,K17,20,28,29,31,32"},
	{"res/DraughtsGame1Move47.jpg", "4,5,19,22,25",                "K2,K17,20,28,29,31,32"},
	{"res/DraughtsGame1Move48.jpg", "4,5,19,25",                   "K2,20,K26,28,29,31,32"},
	{"res/DraughtsGame1Move49.jpg", "4,5,19,K30",                  "K2,20,K26,28,29,31,32"},
	{"res/DraughtsGame1Move50.jpg", "4,5,19,K30",                  "K2,20,K26,27,28,29,32"},
	{"res/DraughtsGame1Move51.jpg", "4,5,19,K23",                  "K2,20,27,28,29,32"},
	{"res/DraughtsGame1Move52.jpg", "4,5,19",                      "K2,18,20,28,29,32"},
	{"res/DraughtsGame1Move53.jpg", "4,5,23",                      "K2,18,20,28,29,32"},
	{"res/DraughtsGame1Move54.jpg", "4,5,23",                      "K2,15,20,28,29,32"},
	{"res/DraughtsGame1Move55.jpg", "4,5,26",                      "K2,15,20,28,29,32"},
	{"res/DraughtsGame1Move56.jpg", "4,5,26",                      "K2,11,20,28,29,32"},
	{"res/DraughtsGame1Move57.jpg", "4,5,K31",                     "K2,11,20,28,29,32"},
	{"res/DraughtsGame1Move58.jpg", "4,5,K31",                     "K2,11,20,27,28,29"},
	{"res/DraughtsGame1Move59.jpg", "4,5,K24",                     "K2,11,20,28,29"},
	{"res/DraughtsGame1Move60.jpg", "4,5",                         "K2,11,19,20,29"},
	{"res/DraughtsGame1Move61.jpg", "4,9",                         "K2,11,19,20,29"},
	{"res/DraughtsGame1Move62.jpg", "4,9",                         "K2,11,19,20,25"},
	{"res/DraughtsGame1Move63.jpg", "4,14",                        "K2,11,19,20,25"},
	{"res/DraughtsGame1Move64.jpg", "4,14",                        "K2,11,19,20,22"},
	{"res/DraughtsGame1Move65.jpg", "4,18",                        "K2,11,19,20,22"},
	{"res/DraughtsGame1Move66.jpg", "4",                           "K2,11,15,19,20"},
	{"res/DraughtsGame1Move67.jpg", "8",                           "K2,11,15,19,20"},
	{"res/DraughtsGame1Move68.jpg", "",                            "K2,K4,15,19,20"}
};

#endif
