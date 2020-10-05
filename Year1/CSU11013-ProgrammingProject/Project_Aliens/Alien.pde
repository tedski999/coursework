
public class Alien
{ 
  protected boolean isAlive;
  protected PVector position;
  protected PImage currentTexture;
  protected PImage aliveTexture;
  protected PImage deadTexture;
  
  public Alien()
  {
    isAlive = true;
  }
  
  public void draw()
  {
    if (isAlive && random(1000) < 1)
      explode();
    
    image(currentTexture, position.x - currentTexture.width / 2, position.y - currentTexture.height / 2);
  }
  
  protected void explode()
  {
    isAlive = false;
    currentTexture = deadTexture;
  }
  
  // Override this
  protected void update() {}
};
