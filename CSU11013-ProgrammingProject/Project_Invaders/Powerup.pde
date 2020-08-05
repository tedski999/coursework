
public class Powerup
{
  public PVector position;
  public PVector velocity;
  public PImage texture;
  public int type;
  
  public Powerup(PVector position, PVector velocity, PImage texture, int type)
  {
    this.position = position;
    this.velocity = velocity;
    this.texture = texture;
    this.type = type;
  }
  
  public boolean checkForCollisionWithPlayer(Player player)
  {
    if (position.x - texture.width / 2 < player.position.x + player.currentTexture.width / 2 &&
        position.x + texture.width / 2 > player.position.x - player.currentTexture.width / 2 &&
        position.y - texture.height / 2 < player.position.y + player.currentTexture.height / 2 &&
        position.y + texture.height / 2 > player.position.y - player.currentTexture.height / 2)
    {
      switch (type)
      {
        case (POWERUP_SPEED):
          speedPowerupTimeRemaining = POWERUP_DURATION;
          break;
        
        case (POWERUP_SPLIT):
          splitPowerupTimeRemaining = POWERUP_DURATION;
          break;
        
        case (POWERUP_RAPID):
          rapidPowerupTimeRemaining = POWERUP_DURATION;
          break;
        
        default:
          break;
      }
      return true;
    }
    else
      return false;
  }
  
  public void update()
  { 
    position.add(velocity);
    if (position.x > SCREEN_SIZE.x - texture.width / 2)
      velocity.x = -abs(velocity.x);
    else if (position.x < texture.width / 2)
      velocity.x = abs(velocity.x);
  }

  public void draw()
  {
    image(texture, position.x, position.y);
  }
}
