
public class Alien
{ 
  public boolean isAlive;
  public PVector position;
  public PVector velocity;
  public float rotation;
  public float torque;
  public PImage currentTexture;
  public int health;

  protected PImage aliveTexture;
  protected PImage deadTexture;
  
  public Alien()
  {
    isAlive = true;
    velocity = new PVector(0, 0);
    rotation = 0;
    torque = 0;
  }
  
  public boolean isOutOfBounds()
  {
    return (position.x > SCREEN_SIZE.x + 1 || position.x < -1 || position.y > SCREEN_SIZE.y + 1 || position.y < -1);
  }
  
  public void draw()
  {
    pushMatrix();
    translate(position.x, position.y);
    rotate(rotation);
    translate(-position.x, -position.y);
    image(currentTexture, position.x, position.y);
    popMatrix();
  }
  
  public void doCollisions()
  {
    if (health <= 0)
      explode();
    
    if (velocity.mag() == 0)
      return;
    
    for (Alien otherAlien : aliens)
    {
      if (otherAlien == this)
        continue;
      
      if (isAlive && otherAlien.isAlive &&
          position.x - currentTexture.width / 2 < otherAlien.position.x + otherAlien.currentTexture.width / 2 &&
          position.x + currentTexture.width / 2 > otherAlien.position.x - otherAlien.currentTexture.width / 2 &&
          position.y - currentTexture.height / 2 < otherAlien.position.y + otherAlien.currentTexture.height / 2 &&
          position.y + currentTexture.height / 2 > otherAlien.position.y - otherAlien.currentTexture.height / 2)
      {
        health--;
        otherAlien.health--;
        PVector displacement = new PVector(position.x - otherAlien.position.x, position.y - otherAlien.position.y);
        displacement = displacement.normalize();
        velocity.add(displacement);
        otherAlien.velocity.add(new PVector(-displacement.x, -displacement.y));
        torque += random(-0.1, 0.1);
        otherAlien.torque += random(-0.1, 0.1);
      }  
    }
    
    rotation += torque;
    position.add(velocity);
    if (position.x > SCREEN_SIZE.x)       velocity.x = -abs(velocity.x);
    else if (position.x < 0)              velocity.x = abs(velocity.x);
    else if (position.y > SCREEN_SIZE.y)  velocity.y = -abs(velocity.y);
    else if (position.y < 0)              velocity.y = abs(velocity.y);
  }
  
  protected void explode()
  {
    if (isAlive)
    {
      isAlive = false;
      currentTexture = deadTexture;
      if (random(1 / POWERUP_CHANCE) < 1)
        spawnPowerup(position.copy());
    }
  }
  
  // Override this
  protected void update() {}
};
