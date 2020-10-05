
public class Player
{
  public PVector position;
  public PImage currentTexture;
  private PImage playerTexture;
  private PImage explosionTexture;
  
  public Player(PVector position, PImage playerTexture, PImage explosionTexture)
  {
    this.position = position;
    this.playerTexture = playerTexture;
    this.explosionTexture = explosionTexture;
    currentTexture = this.playerTexture;
  }
  
  public void update()
  { 
    if (mouseX > SCREEN_SIZE.x - currentTexture.width / 2)
      position.x = SCREEN_SIZE.x - currentTexture.width / 2;
    else if (mouseX < currentTexture.width / 2)
      position.x = currentTexture.width / 2;
    else
      position.x = mouseX;
  }

  public void draw()
  {
    image(currentTexture, position.x, position.y);
  }
}
