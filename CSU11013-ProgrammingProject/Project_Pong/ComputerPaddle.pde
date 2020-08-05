
class ComputerPaddle extends Paddle
{ 
  float difficulty;
  
  void update()
  {
    difficulty += COMPUTER_DIFFICULTY_SPEED;
    float oldXPosition = this.position.x;

    float nextXPosition = this.position.x;
    if (this.position.x + (PADDLE_SIZE.x / 6) < ball.position.x)
      nextXPosition += log(difficulty);
    else if (this.position.x - (PADDLE_SIZE.x / 6) > ball.position.x)
      nextXPosition -= log(difficulty);
    nextXPosition += ball.velocity.x / 10;
    
    if (nextXPosition > width - PADDLE_SIZE.x / 2)
      this.position.x = width - (PADDLE_SIZE.x / 2);
    else if (nextXPosition < PADDLE_SIZE.x / 2)
      this.position.x = PADDLE_SIZE.x / 2;
    else
      this.position.x = nextXPosition;
    
    this.speed = this.position.x - oldXPosition;
  }
}
