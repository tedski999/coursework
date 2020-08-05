// A pong replica with basic AI made for CSU11013 - Programming Project Assignment 2
// Programming credit: Ted Johnson - 2020/01/30
// Icon graphic credit: www.freepik.com/flaticon

// Window constants
final String   WINDOW_TITLE = "Pong - Programming Project - Assignment 2";
final PVector  SCREEN_SIZE = new PVector(640, 480);
final int      FRAMERATE = 60;

// Game constants
final int      SCORE_TO_WIN = 3;
final int      SCORE_TEXT_MARGIN = 25;

// Paddle constants
final PVector  PADDLE_SIZE = new PVector(100, 10);
final int      PADDLE_MARGIN = 50;
final color    PADDLE_COLOR = color(255);

// AI constants
final float    COMPUTER_STARTING_DIFFICULTY = 10f;
final float    COMPUTER_DIFFICULTY_SPEED = 0.005f;

// Ball constants
final float    BALL_STARTING_SPEED = 2;
final float    BALL_SPEED_MULTIPLIER = 1.1f;
final float    BALL_PADDLE_TRAJECTORY_MULTIPLIER = 0.05f;
final int      BALL_RADIUS = 5;
final color    BALL_COLOR = color(200, 100, 50);

// Text constants
final int      SMALL_TEXT_SIZE = 20;
final int      MEDIUM_TEXT_SIZE = 25;
final int      LARGE_TEXT_SIZE = 35;
final color    WHITE_TEXT_COLOR = color(255);
final color    GREEN_TEXT_COLOR = color(0, 255, 0);
final color    YELLOW_TEXT_COLOR = color(255, 255, 0);
final color    RED_TEXT_COLOR = color(255, 0, 0);
final PVector  COMPUTER_DIFFICULTY_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, 20);
final PVector  BALL_SPEED_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, SCREEN_SIZE.y - 20);
final PVector  START_ROUND_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, SCREEN_SIZE.y / 2);
final PVector  ROUND_WINNER_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, SCREEN_SIZE.y / 2);
final PVector  GAME_WINNER_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, (SCREEN_SIZE.y / 2) - 20);
final PVector  GAME_OVER_TEXT_POSITION = new PVector(SCREEN_SIZE.x / 2, (SCREEN_SIZE.y / 2) + 20);

// Global objects
ComputerPaddle computerPaddle;
HumanPaddle humanPaddle;
Ball ball;
boolean roundRunning;
boolean gameRunning;
String endOfRoundText;
String gameWinner;


// Setup Processing window
void settings()
{
  size(
    int(SCREEN_SIZE.x),
    int(SCREEN_SIZE.y));
}

// Setup game objects
void setup()
{
  // Set Processing settings
  surface.setTitle(WINDOW_TITLE);
  surface.setIcon(loadImage("icon.png"));
  ellipseMode(RADIUS);
  textFont(loadFont("LucidaConsole-48.vlw"));
  textAlign(CENTER);
  frameRate(FRAMERATE);
  
  // Instantiate game objects
  computerPaddle = new ComputerPaddle();
  humanPaddle = new HumanPaddle();
  ball = new Ball();
  
  // Sets up the game
  restart();
}

// Main game loop, called every (1 / FRAMERATE) seconds
void draw()
{
  background(0);
  
  if (roundRunning)
    updateRound();
  
  computerPaddle.draw();
  humanPaddle.draw();
  ball.draw();
  
  drawGUI();
}

// Draws text to screen
void drawGUI()
{
  if (gameRunning)
  {
    fill(WHITE_TEXT_COLOR);
    textSize(SMALL_TEXT_SIZE);
    text(
      humanPaddle.score,
      SCORE_TEXT_MARGIN,
      height - SCORE_TEXT_MARGIN);
    text(
      computerPaddle.score,
      width - SCORE_TEXT_MARGIN,
      SCORE_TEXT_MARGIN);
    text(
      "Computer difficulty: " + round((computerPaddle.difficulty - COMPUTER_STARTING_DIFFICULTY) * 10),
      COMPUTER_DIFFICULTY_TEXT_POSITION.x,
      COMPUTER_DIFFICULTY_TEXT_POSITION.y);
    text(
      "Current speed: " + round(ball.velocity.mag() * 10),
      BALL_SPEED_TEXT_POSITION.x,
      BALL_SPEED_TEXT_POSITION.y);
    
    if (!roundRunning)
    {
      textSize(MEDIUM_TEXT_SIZE);
      fill(GREEN_TEXT_COLOR);
      text(
        endOfRoundText + "Click to start the round",
        START_ROUND_TEXT_POSITION.x,
        START_ROUND_TEXT_POSITION.y);
    }
  }
  else
  {
    textSize(LARGE_TEXT_SIZE);
    fill(YELLOW_TEXT_COLOR);
    text(
      gameWinner + " won the game!",
      GAME_WINNER_TEXT_POSITION.x,
      GAME_WINNER_TEXT_POSITION.y);
    
    textSize(MEDIUM_TEXT_SIZE);
    fill(RED_TEXT_COLOR);
    text(
      "Click to play again",
      GAME_OVER_TEXT_POSITION.x,
      GAME_OVER_TEXT_POSITION.y);
  }
}

// Do logic to run round
void updateRound()
{
  // Update game objects
  computerPaddle.update();
  humanPaddle.update();
  ball.update();
  
  // Check if the computer or player has won the round
  if (ball.position.y < 0)
  {
    endOfRoundText = "You got a point!\n";
    humanPaddle.score += 1;
    ball.setVelocity(BALL_STARTING_SPEED, random(-(PI / 2) - 1, -(PI / 2) + 1));
    reset();
  }
  else if (ball.position.y > height)
  {
    endOfRoundText = "The computer got a point!\n";
    computerPaddle.score += 1;
    ball.setVelocity(BALL_STARTING_SPEED, random((PI / 2) - 1, (PI / 2) + 1));
    reset();
  }
  
  // Check if the computer or player has won the game
  if (humanPaddle.score >= SCORE_TO_WIN)
  {
    gameWinner = "You";
    gameRunning = false;
  }
  else if (computerPaddle.score >= SCORE_TO_WIN)
  {
    gameWinner = "The computer";
    gameRunning = false;
  }
}

// Sets up another round
void reset()
{
  roundRunning = false;
  computerPaddle.position = new PVector(width / 2, PADDLE_MARGIN);
  humanPaddle.position = new PVector(width / 2, height - PADDLE_MARGIN);
  ball.position = new PVector(width / 2, height / 2);
}

// Sets the game back to starting conditions
void restart()
{
  reset();
  endOfRoundText = "";
  gameWinner = "";
  computerPaddle.difficulty = COMPUTER_STARTING_DIFFICULTY;
  computerPaddle.score = 0;
  humanPaddle.score = 0;
  ball.setVelocity(BALL_STARTING_SPEED, random((PI / 2) - 1, (PI / 2) + 1));
  gameRunning = true;
}

// Used to start the round or restart the game
void mousePressed()
{
  // Restart the game if clicked while the game is over
  if (!gameRunning)
    restart(); 
  
  // Start the round if the game is clicked while the round is over
  else if (!roundRunning)
    roundRunning = true; 
}
