
class Paddle
{
  PVector position;
  float speed;
  int score;
  
  // Useless constructor, only to prevent accidental NPEs occuring 
  Paddle()
  {
    position = new PVector(0, 0);
    speed = 0f;
    score = 0;
  }

  void draw()
  {
    fill(PADDLE_COLOR);
    rect(
      this.position.x - (PADDLE_SIZE.x / 2),
      this.position.y - (PADDLE_SIZE.y / 2),
      PADDLE_SIZE.x,
      PADDLE_SIZE.y);
  }
  
  // Overriden
  void update() {}
}
