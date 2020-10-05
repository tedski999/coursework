
final float SQUARE_WIDTH = 100f;
final float TOP_SQUARE_SPEED = 3f;
final float BOTTOM_SQUARE_SPEED = -2f;
final float SIN_WAVE_FREQUENCY = 0.1f;
final float SIN_WAVE_MAGNITUDE = 10f;
final int HUE_CHANGE_RATE = 3;

PVector topSquarePosition = new PVector(200, 100);
PVector bottomSquarePosition = new PVector(300, 300);
float sinusoidalOffset = 0;
int hueOffset = 0;

void setup()
{
  size(640, 480);
  frameRate(60);
  noStroke();
  colorMode(HSB, 255);
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
 
  // Update offsets
  sinusoidalOffset = sin(frameCount * SIN_WAVE_FREQUENCY) * SIN_WAVE_MAGNITUDE;
  hueOffset += HUE_CHANGE_RATE;
  if (hueOffset >= 255)
    hueOffset = 0;
  
  // Prepare draw buffer
  background(255);
  fill(hueOffset, 255, 255);
  
  // Draw top square
  rect(topSquarePosition.x, topSquarePosition.y + sinusoidalOffset, SQUARE_WIDTH, SQUARE_WIDTH);
  if (topSquarePosition.x + SQUARE_WIDTH >= width)
    rect(topSquarePosition.x - width, topSquarePosition.y + sinusoidalOffset, SQUARE_WIDTH, SQUARE_WIDTH);
  
  // Draw bottom square
  rect(bottomSquarePosition.x, bottomSquarePosition.y + sinusoidalOffset, SQUARE_WIDTH, SQUARE_WIDTH);
  if (bottomSquarePosition.x <= 0)
    rect(width + bottomSquarePosition.x, bottomSquarePosition.y + sinusoidalOffset, SQUARE_WIDTH, SQUARE_WIDTH);
}

void keyPressed()
{
  if (key == ESC)
    exit();
}
