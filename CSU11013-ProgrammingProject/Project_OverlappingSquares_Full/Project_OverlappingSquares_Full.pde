
final int FRAME_RATE = 60;
final color BACKGROUND_COLOR = #ffffff;
final float MOVING_SQUARES_SIZE = 100;

final PVector RED_SQUARE_STARTING_POSITION = new PVector(-50, -50);
final PVector RED_SQUARE_STARTING_SPEED = new PVector(-0.25, 0);
final color RED_SQUARE_COLOR = #ff0000;
final PVector GREEN_SQUARE_STARTING_POSITION = new PVector(0, 0);
final PVector GREEN_SQUARE_STARTING_SPEED = new PVector(0.25, 0);
final color GREEN_SQUARE_COLOR = #00ff00;
final PVector BLUE_SQUARE_STARTING_POSITION = new PVector(50, 50);
final PVector BLUE_SQUARE_STARTING_SPEED = new PVector(0, 0.25);
final color BLUE_SQUARE_COLOR = #0000ff;

MovingRect redSquare = new MovingRect();
MovingRect blueSquare = new MovingRect();
MovingRect greenSquare = new MovingRect();

void setup()
{
  // Setup the window
  size(640, 480);
  frameRate(FRAME_RATE);
  noStroke();
  
  // Note: Could initialize MovingRects here to be verbose
  // redSquare = new MovingRect();
  // etc...

  // Setup the red moving square
  redSquare.position = RED_SQUARE_STARTING_POSITION;
  redSquare.velocity = RED_SQUARE_STARTING_SPEED;
  redSquare.fillColor = RED_SQUARE_COLOR;
  
  // Setup the green moving square
  greenSquare.position = GREEN_SQUARE_STARTING_POSITION;
  greenSquare.velocity = GREEN_SQUARE_STARTING_SPEED;
  greenSquare.fillColor = GREEN_SQUARE_COLOR;
  
  // Setup the blue moving square
  blueSquare.position = BLUE_SQUARE_STARTING_POSITION;
  blueSquare.velocity = BLUE_SQUARE_STARTING_SPEED;
  blueSquare.fillColor = BLUE_SQUARE_COLOR;
}

void draw()
{
  // Update the positions of the squares
  redSquare.updateRect();
  greenSquare.updateRect();
  blueSquare.updateRect();
  
  // Draw the squares
  background(BACKGROUND_COLOR);
  redSquare.drawRect();
  greenSquare.drawRect();
  blueSquare.drawRect();
}

class MovingRect
{
  PVector position = new PVector(0, 0);
  PVector size = new PVector(MOVING_SQUARES_SIZE, MOVING_SQUARES_SIZE);
  PVector velocity = new PVector(0, 0);
  color fillColor = #000000;
  
  // Note: Could initialize here to be verbose
  // MovingRect() {}
  
  // Offset the position of the rectange by the current velocity
  void updateRect()
  {
    position.add(velocity);
  }
  
  // Draw rectange, with center at 0,0 in the center of the window
  void drawRect()
  {
    fill(fillColor);
    rect(
      (width / 2) + (position.x - size.x / 2),
      (height / 2) + (position.y - size.y / 2),
      size.x,
      size.y);
  }
};
