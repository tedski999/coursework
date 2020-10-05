
public class DropAlien extends Alien
{
  /* private enum AlienState { move, stop }; // Enums not supported in Processing 3.5.4 */
  public final static int STATE_MOVE = 0;
  public final static int STATE_HOLD = 1;
  
  private final float SPEED = 3;
  private final float SHAKE_AMPLITUDE = 3f;
  private final float DROP_DISTANCE = 50;
  private final float HOLD_TIME = 100;
  
  private int currentState;
  private float targetYPosition;
  private float holdTimeLeft;
  
  public DropAlien(PVector position, PImage aliveTexture, PImage deadTexture)
  {
    this.position = position;
    this.aliveTexture = aliveTexture;
    this.deadTexture = deadTexture;
    currentTexture = this.aliveTexture;
    currentState = STATE_HOLD;
  }
  
  @Override
  public void update()
  {
    if (isAlive)
    { 
      switch (currentState)
      {
        case (DropAlien.STATE_MOVE):
          move();
          break;
        
        case (DropAlien.STATE_HOLD):
          hold();
          break;
        
        default:
          break;
      }
    }
  }
  
  private void move()
  {
    position.y += SPEED;
    if (position.y >= targetYPosition)
    {
      holdTimeLeft = HOLD_TIME;
      currentState = DropAlien.STATE_HOLD;
    }
  }
  
  private void hold()
  { 
    position.x += random(-SHAKE_AMPLITUDE, SHAKE_AMPLITUDE);
    if (position.x > SCREEN_SIZE.x - currentTexture.width / 2)
      position.x = SCREEN_SIZE.x - currentTexture.width / 2;
    else if (position.x < currentTexture.width / 2)
      position.x = currentTexture.width / 2;
    
    if (holdTimeLeft-- <= 0)
    {
      targetYPosition = position.y + DROP_DISTANCE;
      currentState = DropAlien.STATE_MOVE;
    }
  }
};
