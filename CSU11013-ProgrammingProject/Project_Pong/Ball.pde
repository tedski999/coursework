
class Ball
{
  PVector position;
  PVector velocity;
  
  // Useless constructor, only to prevent accidental NPEs occuring 
  Ball()
  {
    position = new PVector(0, 0);
    velocity = new PVector(0, 0);
  }

  // Updates the position of the ball
  void update()
  {
    this.position.add(velocity);
    
    // Check for wall collisions
    if (this.position.x + BALL_RADIUS > width)
      this.velocity.x = -abs(velocity.x);
    else if (this.position.x - BALL_RADIUS < 0)
      this.velocity.x = abs(velocity.x);
      
    // Check for paddle and ball collisions
    if (this.isCollidingWithPaddle(computerPaddle))
    {
      this.velocity.x += computerPaddle.speed * BALL_PADDLE_TRAJECTORY_MULTIPLIER;
      this.velocity.y = abs(this.velocity.y);
      this.velocity.mult(BALL_SPEED_MULTIPLIER);
    }
    if (this.isCollidingWithPaddle(humanPaddle))
    {
      this.velocity.x += humanPaddle.speed * BALL_PADDLE_TRAJECTORY_MULTIPLIER;
      this.velocity.y = -abs(ball.velocity.y);
      this.velocity.mult(BALL_SPEED_MULTIPLIER);
    }
  }

  // Draw the ball to the screen
  void draw()
  {
    fill(BALL_COLOR);
    ellipse(this.position.x, this.position.y, BALL_RADIUS, BALL_RADIUS);
  }

  // Checks if colliding with the player
  boolean isCollidingWithPaddle(Paddle target)
  {
    if (this.position.y + BALL_RADIUS >= target.position.y - (PADDLE_SIZE.y / 2) &&
        this.position.y - BALL_RADIUS <= target.position.y + (PADDLE_SIZE.y / 2) &&
        this.position.x + BALL_RADIUS >= target.position.x - (PADDLE_SIZE.x / 2) &&
        this.position.x - BALL_RADIUS <= target.position.x + (PADDLE_SIZE.x / 2))
      return true;
    else
      return false;
  }
  
  void setVelocity(float speed, float direction)
  {
    this.velocity = new PVector(
      cos(direction) * speed,
      sin(direction) * speed);
  }
}
