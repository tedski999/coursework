
PVector squarePosition = new PVector(100, 200);
float squareWidth = 100f;
float squareSpeed = 3f;

void setup()
{
  size(640, 480);
  frameRate(60);
  noStroke();
  fill(255, 0, 0);
}

void draw()
{
  // Update square position
  squarePosition.x += squareSpeed;
  if (squarePosition.x >= width)
    squarePosition.x = -squareWidth;
  
  // Draw square
  background(255);
  rect(squarePosition.x, squarePosition.y, squareWidth, squareWidth);
}

void keyPressed()
{
  if (key == ESC)
    exit();
}
