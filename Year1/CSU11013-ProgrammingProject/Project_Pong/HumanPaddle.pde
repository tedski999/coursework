
class HumanPaddle extends Paddle
{ 
  void update()
  {
    float oldXPosition = this.position.x;
    
    if (mouseX > width - PADDLE_SIZE.x / 2)
      this.position.x = width - (PADDLE_SIZE.x / 2);
    else if (mouseX < PADDLE_SIZE.x / 2)
      this.position.x = PADDLE_SIZE.x / 2;
    else
      this.position.x = mouseX;
      
     this.speed = this.position.x - oldXPosition;
  }
}
