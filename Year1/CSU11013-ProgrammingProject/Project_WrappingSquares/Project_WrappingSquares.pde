
final float SQUARE_WIDTH = 100f;
final float TOP_SQUARE_SPEED = 3f;
final color TOP_SQUARE_COLOR = #ff0000;
final float BOTTOM_SQUARE_SPEED = -2f;
final color BOTTOM_SQUARE_COLOR = #0000ff;

PVector topSquarePosition = new PVector(200, 100);
PVector bottomSquarePosition = new PVector(300, 300);

void setup()
{
  size(640, 480);
  frameRate(60);
  noStroke();
}

void draw()
{
  // Update top square position
  topSquarePosition.x += TOP_SQUARE_SPEED;
  if (topSquarePosition.x >= width)
    topSquarePosition.x = 0;
  
  // Update bottom square position
  bottomSquarePosition.x += BOTTOM_SQUARE_SPEED;
  if (bottomSquarePosition.x + SQUARE_WIDTH <= 0)
    bottomSquarePosition.x = width - SQUARE_WIDTH;
  
  // Prepare draw buffer
  background(255);
  
  // Draw top square
  fill(TOP_SQUARE_COLOR);
  rect(topSquarePosition.x, topSquarePosition.y, SQUARE_WIDTH, SQUARE_WIDTH);
  if (topSquarePosition.x + SQUARE_WIDTH >= width)
    rect(topSquarePosition.x - width, topSquarePosition.y, SQUARE_WIDTH, SQUARE_WIDTH);
  
  // Draw bottom square
  fill(BOTTOM_SQUARE_COLOR);
  rect(bottomSquarePosition.x, bottomSquarePosition.y, SQUARE_WIDTH, SQUARE_WIDTH);
  if (bottomSquarePosition.x <= 0)
    rect(width + bottomSquarePosition.x, bottomSquarePosition.y, SQUARE_WIDTH, SQUARE_WIDTH);
}

void keyPressed()
{
  if (key == ESC)
    exit();
}
