
PVector redSquarePosition = new PVector((640 / 2) - 50, (480 / 2) - 50);
PVector greenSquarePosition = new PVector(640 / 2, 480 / 2);
PVector blueSquarePosition = new PVector((640 / 2) + 50, (480 / 2) + 50);

void setup()
{
  size(640, 480);
  frameRate(60);
  noStroke();
}

void draw()
{
  // Update square positions
  redSquarePosition.x -= 0.25f;
  greenSquarePosition.x += 0.25f;
  blueSquarePosition.y += 0.25f;
  
  // Draw squares
  background(255);
  fill(255, 0, 0);
  rect(redSquarePosition.x - 50, redSquarePosition.y - 50, 100, 100);
  fill(0, 255, 0);
  rect(greenSquarePosition.x - 50, greenSquarePosition.y - 50, 100, 100);
  fill(0, 0, 255);
  rect(blueSquarePosition.x - 50, blueSquarePosition.y - 50, 100, 100); 
}

void keyPressed()
{
  if (key == ESC)
    exit();
}
