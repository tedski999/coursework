
PVector squarePosition = new PVector(300, 200);
float squareWidth = 100f;
float squareSpeed = 2f;

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
    squarePosition.x = 0;
  
  // Prepare draw buffer
  background(255);
  
  // Draw square
  rect(squarePosition.x, squarePosition.y, squareWidth, squareWidth);
  if (squarePosition.x + squareWidth >= width)
    rect(squarePosition.x - width, squarePosition.y, squareWidth, squareWidth);
}

void keyPressed()
{
  if (key == ESC)
    exit();
}
