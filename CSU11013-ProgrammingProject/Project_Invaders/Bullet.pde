
public class Bullet
{
  private final PVector SIZE = new PVector(5, 5);
  
  public PVector position;
  public PVector velocity;
  
  public Bullet(PVector position, PVector velocity)
  {
    this.position = position;
    this.velocity = velocity;
  }

  public void checkForCollisionWithAliens(Alien[] aliens)
  {
    for (Alien alien : aliens)
    {
      if (alien.isAlive &&
          position.x - SIZE.x / 2 < alien.position.x + alien.currentTexture.width / 2 &&
          position.x + SIZE.x / 2 > alien.position.x - alien.currentTexture.width / 2 &&
          position.y - SIZE.y / 2 < alien.position.y + alien.currentTexture.height / 2 &&
          position.y + SIZE.y / 2 > alien.position.y - alien.currentTexture.height / 2)
      {
        alien.velocity.add(new PVector(random(-2, 2), random(-4, -2)));
        velocity.add(new PVector(random(-2, 2), random(4, 2)));
        alien.torque += random(-0.1, 0.1);
      } 
    }
  }
  
  public boolean isOutOfBounds()
  {
    return (position.x > SCREEN_SIZE.x || position.x < 0 || position.y > SCREEN_SIZE.y || position.y < 0);
  }
  
  public void update()
  {
    position.add(velocity);
  }

  public void draw()
  {
    fill(255);
    rect(position.x - SIZE.x / 2, position.y - SIZE.x / 2, SIZE.x, SIZE.y);
  }
}
